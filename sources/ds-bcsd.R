.pull_bcsd <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_collection(
        url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/bcsd_obs",
        id  = "bcsd"
    ))
}

.tidy_bcsd <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(variable = varname, tiled = "", type = "zarr")
}

ds_bcsd <- climateR.catalogs::data_source$new(
    id   = "bcsd",
    pull = .pull_bcsd,
    tidy = .tidy_bcsd
)
