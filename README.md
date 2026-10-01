# AORC polygon downloader

Downloads an hourly, polygon-clipped subset of NOAA's AORC v1.1 30-arc-second (approximately 1 km) CONUS dataset. The source is the public annual Zarr archive in `s3://noaa-nws-aorc-v1-1-1km`.

The workflow queries the bucket and annual Zarr metadata before downloading. It reads only Zarr chunks intersecting the requested dates, variables, and AOI bounding box, masks the result to the AOI polygon, and writes compressed monthly NetCDF files.

## Files

- `download_aorc.R`: command-line entry point and validation
- `config.example.yml`: configurable dates, shapefile, variables, and output

## Requirements

Install CRAN packages:

```r
install.packages(c("httr2", "jsonlite", "xml2", "sf", "yaml", "terra", "ncdf4"))
```

Install `Rarr` from Bioconductor:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rarr")
```

The workflow is entirely R-based. `Rarr` reads selected chunks directly from the public Zarr arrays; no Python installation or AWS credentials are required.

If `BiocManager` reports that the Bioconductor version cannot be validated, set
the repository explicitly for R 4.5 and retry:

```r
options(repos = c(
  CRAN = "https://cloud.r-project.org",
  BioC = "https://bioconductor.org/packages/3.22/bioc"
))
install.packages("Rarr")
```

## Configure

Copy `config.example.yml` to `config.yml`, then edit:

- `shapefile`: polygon dataset with a defined CRS
- `start_datetime` and `end_datetime`: inclusive hourly UTC bounds
- `variables`: required AORC variables only
- `output_directory`: destination directory

Supported variables:

| Variable | Meaning | Units |
|---|---|---|
| `APCP_surface` | Hourly total precipitation | kg m-2 (equivalent to mm) |
| `TMP_2maboveground` | 2 m air temperature | K |
| `SPFH_2maboveground` | 2 m specific humidity | kg kg-1 |
| `PRES_surface` | Surface pressure | Pa |
| `DLWRF_surface` | Downward longwave radiation | W m-2 |
| `DSWRF_surface` | Downward shortwave radiation | W m-2 |
| `UGRD_10maboveground` | Eastward wind component | m s-1 |
| `VGRD_10maboveground` | Northward wind component | m s-1 |

## Check availability first

Run a metadata-only check:

```powershell
Rscript download_aorc.R --config config.yml --check-only
```

This performs the following without downloading array data:

1. validates and reprojects the polygon to EPSG:4326;
2. lists year prefixes from the unsigned S3 `ListObjectsV2` endpoint;
3. confirms each annual `.zmetadata` object and requested variables;
4. reads annual time coordinates and checks hourly continuity;
5. compares requested dates against actual source coverage;
6. prints a JSON metadata report.

Do not infer availability only from the current calendar date. AORC is produced with a lag, and annual stores can be incomplete or regenerated.

Successful checks are cached by default in `.aorc_metadata_cache.rds` under the
output directory. The cache includes the bucket listing, annual Zarr metadata,
and time/latitude/longitude coordinates, so restarting does not repeat those
remote reads. Historical entries are reused for 168 hours by default; the
newest annual store is refreshed after 6 hours because it may still be changing.

Cache settings:

- `metadata_cache`: enable or disable the persistent cache;
- `metadata_cache_path`: optional custom cache path;
- `metadata_cache_ttl_hours`: lifetime for historical annual checks;
- `latest_year_cache_ttl_hours`: lifetime for the newest annual store and bucket listing;
- `refresh_metadata_cache`: force a remote refresh for the current run.

The cache is updated after each successful annual check, so an interrupted run
can reuse all years already checked. A TTL of `0` always performs a fresh check.

## Download

```powershell
Rscript download_aorc.R --config config.yml
```

The script writes one compressed NetCDF file per month and then writes `download_manifest.json`. Existing monthly files are skipped unless `overwrite: true`.

Example output:

```text
aorc_output/
  aorc_202405.nc
  download_manifest.json
```

## Disk and network behavior

The annual AORC arrays are chunked approximately as 144 hours × 128 latitude cells × 256 longitude cells. A small request must transfer any source chunks it intersects, so transferred bytes can exceed the final clipped file size. Limiting variables and processing monthly prevents complete annual CONUS arrays from being materialized locally or in memory.

The output retains the rectangular AOI bounding box but cells outside the polygon are set to missing. This is the standard representation for polygon-clipped gridded NetCDF data.

## Availability and data-quality note

At the time this project was prepared, the public bucket exposed annual stores from 1979 through 2025. The script always discovers this dynamically. NOAA's registry posted an April 2, 2026 notice that roughly 0.04% of rows had been incorrectly masked and that Zarr files were being regenerated. Consult the current [NOAA Open Data Registry entry](https://registry.opendata.aws/noaa-nws-aorc/) before scientific production use.
