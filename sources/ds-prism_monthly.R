.pull_prism_monthly <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_collection(
        url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/PRISM_v2",
        id  = "prism_monthly"
    ))
}

.tidy_prism_monthly <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(variable = varname, tiled = "", type = "zarr") |>
        arrow::as_arrow_table()
}

ds_prism_monthly <- climateR.catalogs::data_source$new(
    id = "prism_monthly",
    pull = .pull_prism_monthly,
    tidy = .tidy_prism_monthly
)
