#!/usr/bin/env Rscript

# Download spatially and temporally subsetted NOAA AORC v1.1 data.
# Rarr reads only the remote Zarr chunks intersecting the requested indices.

required_r_packages <- c("httr2", "jsonlite", "xml2", "sf", "yaml", "Rarr", "terra", "ncdf4")
missing_r <- required_r_packages[!vapply(required_r_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_r)) {
  stop(
    "Missing R packages: ", paste(missing_r, collapse = ", "),
    "\nInstall them with install.packages(c(",
    paste(sprintf('"%s"', missing_r), collapse = ", "), ")).",
    call. = FALSE
  )
}

AORC_BUCKET <- "noaa-nws-aorc-v1-1-1km"
AORC_HTTP <- paste0("https://", AORC_BUCKET, ".s3.amazonaws.com")
AORC_S3 <- paste0("s3://", AORC_BUCKET)
AORC_VARIABLES <- c(
  "APCP_surface", "TMP_2maboveground", "SPFH_2maboveground",
  "PRES_surface", "DLWRF_surface", "DSWRF_surface",
  "UGRD_10maboveground", "VGRD_10maboveground"
)

abort <- function(...) stop(paste0(...), call. = FALSE)
messagef <- function(fmt, ...) message(sprintf(fmt, ...))
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

parse_cli <- function(args) {
  result <- list(config = "config.yml", check_only = FALSE)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--check-only") {
      result$check_only <- TRUE
      i <- i + 1L
    } else if (arg == "--config") {
      if (i == length(args)) abort("--config requires a path")
      result$config <- args[[i + 1L]]
      i <- i + 2L
    } else if (arg == "--help") {
      cat("Usage: Rscript download_aorc.R [--config FILE] [--check-only]\n")
      quit(status = 0L)
    } else {
      abort("Unknown argument: ", arg)
    }
  }
  result
}

read_config <- function(path, cli_check_only) {
  if (!file.exists(path)) abort("Configuration file not found: ", path)
  cfg <- yaml::read_yaml(path)
  defaults <- list(
    variables = AORC_VARIABLES,
    output_directory = "aorc_output",
    buffer_km = 0,
    overwrite = FALSE,
    partial_ok = FALSE,
    check_only = FALSE,
    output_prefix = "aorc",
    compression_level = 4L,
    metadata_cache = TRUE,
    metadata_cache_path = NULL,
    metadata_cache_ttl_hours = 168,
    latest_year_cache_ttl_hours = 6,
    refresh_metadata_cache = FALSE
  )
  cfg <- utils::modifyList(defaults, cfg)
  cfg$check_only <- isTRUE(cfg$check_only) || cli_check_only

  required <- c("shapefile", "start_datetime", "end_datetime")
  absent <- required[!vapply(required, function(x) !is.null(cfg[[x]]) && nzchar(as.character(cfg[[x]])), logical(1))]
  if (length(absent)) abort("Missing configuration values: ", paste(absent, collapse = ", "))

  cfg$variables <- unlist(cfg$variables, use.names = FALSE)
  invalid_vars <- setdiff(cfg$variables, AORC_VARIABLES)
  if (length(invalid_vars)) abort("Unknown AORC variables: ", paste(invalid_vars, collapse = ", "))
  if (!length(cfg$variables)) abort("At least one variable must be selected")

  cfg$start <- as.POSIXct(cfg$start_datetime, tz = "UTC", tryFormats = c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"
  ))
  cfg$end <- as.POSIXct(cfg$end_datetime, tz = "UTC", tryFormats = c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"
  ))
  if (is.na(cfg$start) || is.na(cfg$end)) abort("Dates must be valid UTC ISO-8601 values")
  if (cfg$start > cfg$end) abort("start_datetime must not be later than end_datetime")
  if (as.numeric(cfg$start) %% 3600 != 0 || as.numeric(cfg$end) %% 3600 != 0) {
    abort("AORC is hourly; start_datetime and end_datetime must fall exactly on an hour")
  }
  cfg$compression_level <- as.integer(cfg$compression_level)
  if (is.na(cfg$compression_level) || cfg$compression_level < 0L || cfg$compression_level > 9L) {
    abort("compression_level must be between 0 and 9")
  }
  cfg$metadata_cache_ttl_hours <- as.numeric(cfg$metadata_cache_ttl_hours)
  cfg$latest_year_cache_ttl_hours <- as.numeric(cfg$latest_year_cache_ttl_hours)
  if (is.na(cfg$metadata_cache_ttl_hours) || cfg$metadata_cache_ttl_hours < 0) {
    abort("metadata_cache_ttl_hours must be zero or greater")
  }
  if (is.na(cfg$latest_year_cache_ttl_hours) || cfg$latest_year_cache_ttl_hours < 0) {
    abort("latest_year_cache_ttl_hours must be zero or greater")
  }
  cfg
}

request_retry <- function(url) {
  req <- httr2::request(url) |>
    httr2::req_user_agent("download-aorc-r/1.0") |>
    httr2::req_retry(max_tries = 5, backoff = ~ min(30, 2^.x)) |>
    httr2::req_timeout(60)
  tryCatch(httr2::req_perform(req), error = function(e) abort("Request failed for ", url, ": ", conditionMessage(e)))
}

new_metadata_cache <- function() {
  list(schema_version = 1L, source_bucket = AORC_BUCKET, available_years = NULL, years = list())
}

read_metadata_cache <- function(path) {
  if (!file.exists(path)) return(new_metadata_cache())
  cache <- tryCatch(readRDS(path), error = function(e) NULL)
  if (
    is.null(cache) || !identical(cache$schema_version, 1L) ||
      !identical(cache$source_bucket, AORC_BUCKET) || !is.list(cache$years)
  ) {
    message("Ignoring incompatible or unreadable metadata cache: ", path)
    return(new_metadata_cache())
  }
  cache
}

write_metadata_cache <- function(cache, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("aorc-metadata-cache-", tmpdir = dirname(path), fileext = ".rds.part")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(cache, temporary, version = 3)
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) abort("Could not finalize metadata cache: ", path)
}

cache_entry_fresh <- function(entry, ttl_hours) {
  if (is.null(entry) || is.null(entry$checked_utc) || ttl_hours <= 0) return(FALSE)
  checked <- as.POSIXct(entry$checked_utc, tz = "UTC")
  !is.na(checked) && as.numeric(difftime(Sys.time(), checked, units = "hours")) <= ttl_hours
}

query_available_years <- function() {
  url <- paste0(AORC_HTTP, "/?list-type=2&delimiter=%2F&max-keys=1000")
  response <- request_retry(url)
  doc <- xml2::read_xml(httr2::resp_body_string(response))
  prefixes <- xml2::xml_text(xml2::xml_find_all(doc, ".//*[local-name()='CommonPrefixes']/*[local-name()='Prefix']"))
  years <- as.integer(sub("\\.zarr/$", "", grep("^[0-9]{4}\\.zarr/$", prefixes, value = TRUE)))
  years <- sort(unique(years[!is.na(years)]))
  if (!length(years)) abort("No annual Zarr stores were found in the public AORC bucket")
  years
}

read_year_metadata <- function(year) {
  url <- sprintf("%s/%d.zarr/.zmetadata", AORC_HTTP, year)
  response <- request_retry(url)
  metadata <- jsonlite::fromJSON(httr2::resp_body_string(response), simplifyVector = FALSE)$metadata
  names_present <- sub("/\\.zarray$", "", grep("/\\.zarray$", names(metadata), value = TRUE))
  absent <- setdiff(c("time", "latitude", "longitude"), names_present)
  if (length(absent)) abort("Year ", year, " is missing arrays: ", paste(absent, collapse = ", "))

  list(
    year = year,
    url = url,
    variables = intersect(AORC_VARIABLES, names_present),
    raw = metadata
  )
}

validate_year_metadata <- function(metadata, variables) {
  absent <- setdiff(variables, metadata$variables)
  if (length(absent)) abort("Year ", metadata$year, " is missing arrays: ", paste(absent, collapse = ", "))
  array <- metadata$raw[[paste0(variables[[1]], "/.zarray")]]
  metadata$dimensions <- unlist(array$shape)
  metadata$chunks <- unlist(array$chunks)
  metadata
}

prepare_aoi <- function(path, buffer_km) {
  if (!file.exists(path)) abort("Shapefile does not exist: ", path)
  aoi <- suppressWarnings(sf::st_read(path, quiet = TRUE))
  if (!nrow(aoi)) abort("The shapefile contains no features")
  if (is.na(sf::st_crs(aoi))) abort("The shapefile has no coordinate reference system")
  aoi <- sf::st_make_valid(aoi)
  aoi <- aoi[!sf::st_is_empty(aoi), , drop = FALSE]
  if (!nrow(aoi)) abort("The shapefile has no non-empty geometry")

  if (buffer_km > 0) {
    aoi <- sf::st_transform(aoi, 5070)
    aoi <- sf::st_buffer(aoi, as.numeric(buffer_km) * 1000)
  }
  aoi <- sf::st_transform(aoi, 4326)
  aoi <- sf::st_union(aoi)
  bbox <- sf::st_bbox(aoi)
  grid_bbox <- c(xmin = -125, ymin = 25, xmax = -67, ymax = 53)
  intersects <- bbox[["xmax"]] >= grid_bbox[["xmin"]] && bbox[["xmin"]] <= grid_bbox[["xmax"]] &&
    bbox[["ymax"]] >= grid_bbox[["ymin"]] && bbox[["ymin"]] <= grid_bbox[["ymax"]]
  if (!intersects) abort("The area of interest does not intersect the documented CONUS AORC domain")

  temp_gpkg <- tempfile("aorc-aoi-", fileext = ".gpkg")
  sf::st_write(sf::st_sf(id = 1L, geometry = aoi), temp_gpkg, quiet = TRUE, delete_dsn = TRUE)
  list(path = normalizePath(temp_gpkg, winslash = "/", mustWork = TRUE), bbox = bbox)
}

read_zarr_vector <- function(year, name) {
  url <- sprintf("%s/%d.zarr/%s/", AORC_HTTP, year, name)
  as.vector(Rarr::read_zarr_array(url))
}

inspect_year_times <- function(year) {
  times <- read_zarr_vector(year, "time")
  latitude <- read_zarr_vector(year, "latitude")
  longitude <- read_zarr_vector(year, "longitude")
  if (!length(times)) abort("The ", year, " time coordinate is empty")
  list(
    first = as.POSIXct(times[[1]], origin = "1970-01-01", tz = "UTC"),
    last = as.POSIXct(times[[length(times)]], origin = "1970-01-01", tz = "UTC"),
    hours = length(times),
    irregular = length(times) > 1L && any(diff(times) != 3600),
    extent = c(
      xmin = min(longitude), ymin = min(latitude),
      xmax = max(longitude), ymax = max(latitude)
    ),
    time = times,
    latitude = latitude,
    longitude = longitude
  )
}

make_polygon_mask <- function(longitude, latitude, aoi_path) {
  if (length(longitude) < 2L || length(latitude) < 2L) {
    abort("The AOI must intersect at least two AORC cells in each spatial dimension")
  }
  dx <- median(abs(diff(longitude)))
  dy <- median(abs(diff(latitude)))
  template <- terra::rast(
    nrows = length(latitude), ncols = length(longitude),
    xmin = min(longitude) - dx / 2, xmax = max(longitude) + dx / 2,
    ymin = min(latitude) - dy / 2, ymax = max(latitude) + dy / 2,
    crs = "EPSG:4326"
  )
  terra::values(template) <- 1
  masked <- terra::mask(template, terra::vect(aoi_path), touches = TRUE)
  mask <- !is.na(terra::as.matrix(masked, wide = TRUE))
  if (latitude[[1]] < latitude[[length(latitude)]]) mask <- mask[rev(seq_len(nrow(mask))), , drop = FALSE]
  mask
}

write_netcdf_subset <- function(year, block_start, block_end, variables, aoi_path,
                                bbox, output_path, compression_level, metadata, coordinates) {
  time_idx <- which(coordinates$time >= as.numeric(block_start) & coordinates$time <= as.numeric(block_end))
  lat_idx <- which(coordinates$latitude >= bbox[["ymin"]] & coordinates$latitude <= bbox[["ymax"]])
  lon_idx <- which(coordinates$longitude >= bbox[["xmin"]] & coordinates$longitude <= bbox[["xmax"]])
  if (!length(time_idx)) abort("No source hours intersect the requested output block")
  if (!length(lat_idx) || !length(lon_idx)) abort("No AORC grid cells intersect the AOI bounding box")

  latitude <- coordinates$latitude[lat_idx]
  longitude <- coordinates$longitude[lon_idx]
  mask <- make_polygon_mask(longitude, latitude, aoi_path)
  if (!any(mask)) abort("No AORC grid cells intersect the polygon")
  mask_lon_lat <- t(mask)

  lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", longitude)
  lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", latitude)
  time_dim <- ncdf4::ncdim_def(
    "time", "seconds since 1970-01-01 00:00:00 UTC", coordinates$time[time_idx],
    unlim = TRUE, calendar = "proleptic_gregorian"
  )
  fill_value <- -9999
  var_defs <- lapply(variables, function(name) {
    attrs <- metadata$raw[[paste0(name, "/.zattrs")]]
    ncdf4::ncvar_def(
      name, attrs$units %||% "", list(lon_dim, lat_dim, time_dim),
      missval = fill_value, longname = attrs$long_name %||% name,
      prec = "float", compression = compression_level, shuffle = FALSE,
      chunksizes = c(min(256L, length(lon_idx)), min(128L, length(lat_idx)), min(24L, length(time_idx)))
    )
  })

  temporary <- paste0(output_path, ".part")
  if (file.exists(temporary)) unlink(temporary)
  nc <- ncdf4::nc_create(temporary, var_defs, force_v4 = TRUE)
  completed <- FALSE
  on.exit({
    if (!is.null(nc)) try(ncdf4::nc_close(nc), silent = TRUE)
    if (!completed && file.exists(temporary)) unlink(temporary)
  }, add = TRUE)

  for (name in variables) {
    message("  Reading remote chunks for ", name, "...")
    url <- sprintf("%s/%d.zarr/%s/", AORC_HTTP, year, name)
    values <- Rarr::read_zarr_array(url, index = list(time_idx, lat_idx, lon_idx))
    attrs <- metadata$raw[[paste0(name, "/.zattrs")]]
    values[values == (attrs$missing_value %||% -32767)] <- NA_real_
    values <- as.numeric(values) * (attrs$scale_factor %||% 1) + (attrs$add_offset %||% 0)
    dim(values) <- c(length(time_idx), length(lat_idx), length(lon_idx))
    values <- aperm(values, c(3, 2, 1))
    values[rep(!mask_lon_lat, times = length(time_idx))] <- NA_real_
    ncdf4::ncvar_put(nc, name, values)
    ncdf4::ncatt_put(nc, name, "source_name", name)
    ncdf4::ncatt_put(nc, name, "aorc_version", attrs$aorc_version %||% "v1.1")
  }
  ncdf4::ncatt_put(nc, 0, "title", "Spatial and temporal subset of NOAA AORC version 1.1")
  ncdf4::ncatt_put(nc, 0, "source", sprintf("%s/%d.zarr", AORC_HTTP, year))
  ncdf4::ncatt_put(nc, 0, "history", paste("Created by download_aorc.R at", iso_hour(Sys.time()), "UTC"))
  ncdf4::nc_close(nc)
  nc <- NULL
  completed <- TRUE
  if (!file.rename(temporary, output_path)) abort("Could not finalize output: ", output_path)
  list(hours = length(time_idx), latitude_cells = length(lat_idx), longitude_cells = length(lon_idx))
}

monthly_blocks <- function(start, end) {
  starts <- seq(
    as.POSIXct(format(start, "%Y-%m-01 00:00:00", tz = "UTC"), tz = "UTC"),
    as.POSIXct(format(end, "%Y-%m-01 00:00:00", tz = "UTC"), tz = "UTC"),
    by = "month"
  )
  lapply(starts, function(month_start) {
    next_month <- seq(month_start, by = "month", length.out = 2L)[[2L]]
    list(start = max(start, month_start), end = min(end, next_month - 3600))
  })
}

iso_hour <- function(x) format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC")

main <- function() {
  cli <- parse_cli(commandArgs(trailingOnly = TRUE))
  cfg <- read_config(cli$config, cli$check_only)
  aoi <- prepare_aoi(cfg$shapefile, cfg$buffer_km)
  on.exit(unlink(aoi$path), add = TRUE)

  cache_path <- cfg$metadata_cache_path %||% file.path(cfg$output_directory, ".aorc_metadata_cache.rds")
  cache_enabled <- isTRUE(cfg$metadata_cache)
  cache <- if (cache_enabled) read_metadata_cache(cache_path) else new_metadata_cache()
  use_cache <- cache_enabled && !isTRUE(cfg$refresh_metadata_cache)

  if (use_cache && cache_entry_fresh(cache$available_years, cfg$latest_year_cache_ttl_hours)) {
    available_years <- cache$available_years$years
    message("Using cached AORC bucket listing.")
  } else {
    message("Querying the public AORC bucket...")
    available_years <- query_available_years()
    if (cache_enabled) {
      cache$available_years <- list(checked_utc = Sys.time(), years = available_years)
      write_metadata_cache(cache, cache_path)
    }
  }
  requested_years <- seq(as.integer(format(cfg$start, "%Y", tz = "UTC")), as.integer(format(cfg$end, "%Y", tz = "UTC")))
  missing_years <- setdiff(requested_years, available_years)
  internal_missing <- missing_years[missing_years > min(available_years) & missing_years < max(available_years)]
  if (length(internal_missing)) {
    abort("The archive has an internal gap for requested years: ", paste(internal_missing, collapse = ", "))
  }
  if (length(missing_years) && !isTRUE(cfg$partial_ok)) {
    abort("Requested years are unavailable: ", paste(missing_years, collapse = ", "))
  }
  years <- intersect(requested_years, available_years)
  if (!length(years)) abort("None of the requested years are available")

  metadata <- list()
  coverage <- list()
  for (year in years) {
    key <- as.character(year)
    ttl <- if (year == max(available_years)) cfg$latest_year_cache_ttl_hours else cfg$metadata_cache_ttl_hours
    cached <- cache$years[[key]]
    if (use_cache && cache_entry_fresh(cached, ttl)) {
      messagef("Using cached metadata and coordinates for %d.", year)
      metadata[[key]] <- validate_year_metadata(cached$metadata, cfg$variables)
      coverage[[key]] <- cached$coverage
    } else {
      messagef("Checking metadata and time coordinates for %d...", year)
      metadata[[key]] <- validate_year_metadata(read_year_metadata(year), cfg$variables)
      coverage[[key]] <- inspect_year_times(year)
      if (coverage[[key]]$irregular) abort("Year ", year, " has a non-hourly or gapped time coordinate")
      if (cache_enabled) {
        cache$years[[key]] <- list(
          checked_utc = Sys.time(),
          metadata = metadata[[key]],
          coverage = coverage[[key]]
        )
        write_metadata_cache(cache, cache_path)
      }
    }
    if (coverage[[key]]$irregular) abort("Year ", year, " has a non-hourly or gapped time coordinate")
  }

  available_start <- min(vapply(coverage, function(x) as.numeric(x$first), numeric(1)))
  available_end <- max(vapply(coverage, function(x) as.numeric(x$last), numeric(1)))
  available_start <- as.POSIXct(available_start, origin = "1970-01-01", tz = "UTC")
  available_end <- as.POSIXct(available_end, origin = "1970-01-01", tz = "UTC")
  effective_start <- max(cfg$start, available_start)
  effective_end <- min(cfg$end, available_end)
  complete <- cfg$start >= available_start && cfg$end <= available_end && !length(missing_years)
  if (!complete && !isTRUE(cfg$partial_ok)) {
    abort(
      "Requested range is not fully available. Available intersection: ",
      iso_hour(effective_start), " through ", iso_hour(effective_end),
      ". Set partial_ok: true to accept the intersection."
    )
  }
  if (effective_start > effective_end) abort("The requested range has no available hours")

  bbox <- as.numeric(aoi$bbox[c("xmin", "ymin", "xmax", "ymax")])
  names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  messagef("Available bucket years: %d-%d", min(available_years), max(available_years))
  messagef("Requested UTC range: %s through %s", iso_hour(cfg$start), iso_hour(cfg$end))
  messagef("Effective UTC range: %s through %s", iso_hour(effective_start), iso_hour(effective_end))
  messagef("AOI bounding box: %.6f, %.6f, %.6f, %.6f", bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  message("Variables: ", paste(cfg$variables, collapse = ", "))

  manifest <- list(
    created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source_bucket = AORC_BUCKET,
    requested_start = iso_hour(cfg$start),
    requested_end = iso_hour(cfg$end),
    effective_start = iso_hour(effective_start),
    effective_end = iso_hour(effective_end),
    complete_request = complete,
    available_years = available_years,
    variables = cfg$variables,
    aoi_bbox_epsg4326 = as.list(bbox),
    annual_metadata = metadata,
    annual_coverage = lapply(coverage, function(x) list(
      first = iso_hour(x$first), last = iso_hour(x$last), hours = x$hours,
      irregular = x$irregular, extent = as.list(x$extent)
    )),
    outputs = list()
  )

  if (isTRUE(cfg$check_only)) {
    cat(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), "\n")
    message("Metadata check completed; no data were downloaded.")
    return(invisible(NULL))
  }

  dir.create(cfg$output_directory, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(cfg$output_directory, winslash = "/", mustWork = TRUE)
  blocks <- monthly_blocks(effective_start, effective_end)
  for (block in blocks) {
    year <- as.integer(format(block$start, "%Y", tz = "UTC"))
    output_name <- sprintf("%s_%s.nc", cfg$output_prefix, format(block$start, "%Y%m", tz = "UTC"))
    output_path <- file.path(output_dir, output_name)
    if (file.exists(output_path) && !isTRUE(cfg$overwrite)) {
      message("Skipping existing output: ", output_path)
      manifest$outputs[[length(manifest$outputs) + 1L]] <- list(path = output_path, status = "skipped_existing")
      next
    }
    messagef("Writing %s through %s to %s", iso_hour(block$start), iso_hour(block$end), output_path)
    result <- write_netcdf_subset(
      year,
      block$start,
      block$end,
      cfg$variables,
      aoi$path,
      bbox,
      normalizePath(output_path, winslash = "/", mustWork = FALSE),
      cfg$compression_level,
      metadata[[as.character(year)]],
      coverage[[as.character(year)]]
    )
    if (!file.exists(output_path) || file.info(output_path)$size <= 0) abort("Output validation failed: ", output_path)
    manifest$outputs[[length(manifest$outputs) + 1L]] <- list(
      path = output_path,
      status = "written",
      bytes = unname(file.info(output_path)$size),
      hours = result$hours,
      latitude_cells = result$latitude_cells,
      longitude_cells = result$longitude_cells
    )
  }

  manifest_path <- file.path(output_dir, "download_manifest.json")
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), manifest_path)
  message("Completed. Manifest: ", manifest_path)
}

tryCatch(main(), error = function(e) {
  message("ERROR: ", conditionMessage(e))
  quit(status = 1L)
})
