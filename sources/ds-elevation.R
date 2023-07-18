.pull_elevation <- function(...) {
    # nolint start: line_length_linter
    meta <- dplyr::tribble(
        ~id,         ~asset,           ~URL,
        "USGS 3DEP", "30m CONUS DEM",  "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt",
        "USGS 3DEP", "10m CONUS DEM",  "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
        "USGS 3DEP", "60m Alaska DEM", "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/2/TIFF/USGS_Seamless_DEM_2.vrt",
        "GEBCO2019", "Global DEM",     "/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2019_GEOTIFF/GEBCO_2019.tif",
        "NASADEM",   "Global DEM",     "/vsicurl/https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt"
    )
    # nolint end: line_length_linter

    arrow::as_arrow_table(dplyr::mutate(
        meta,
        varname     = "elevation",
        variable    = "elevation",
        units       = "meters",
        description = asset,
        type        = "vrt",
        tiled       = ""
    ))
}

.tidy_elevation <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        climateR.catalogs::vrt_meta(all = TRUE) |>
        arrow::as_arrow_table()
}

ds_elevation <- climateR.catalogs::data_source$new(
    id   = "elevation",
    pull = .pull_elevation,
    tidy = .tidy_elevation
)
