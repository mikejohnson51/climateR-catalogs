# DEFERRED: dcp_compressed STAC collection has no cube:variables and no zarr store.
# Only netcdf files on S3 (s3://mdmf/gdp/netcdf/dcp_compressed/).
# The old cida.usgs.gov THREDDS endpoint is retired.
# To re-enable, either:
#   1. Add cube:variables to the STAC metadata upstream, or
#   2. Parse netcdf files directly from S3

# .pull_dcp <- function(...) { ... }
# .tidy_dcp <- function(.tbl, ...) { ... }
# ds_dcp <- climateR.catalogs::data_source$new(
#     id   = "dcp",
#     pull = .pull_dcp,
#     tidy = .tidy_dcp
# )
