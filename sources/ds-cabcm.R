# DEFERRED: CA-BCM-2014 STAC collection has no cube:variables and no zarr store.
# Only netcdf files on S3 (s3://mdmf/gdp/netcdf/CA-BCM-2014/).
# The old cida.usgs.gov THREDDS endpoint is retired.
# To re-enable, either:
#   1. Add cube:variables to the STAC metadata upstream, or
#   2. Parse netcdf files directly from S3

# .pull_cabcm <- function(...) { ... }
# .tidy_cabcm <- function(.tbl, ...) { ... }
# ds_cabcm <- climateR.catalogs::data_source$new(
#     id   = "cabcm",
#     pull = .pull_cabcm,
#     tidy = .tidy_cabcm
# )
