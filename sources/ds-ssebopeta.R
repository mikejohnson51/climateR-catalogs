.pull_ssebopeta <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_children(
        parent_url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/ssebopeta",
        id = "ssebopeta"
    ))
}

.tidy_ssebopeta <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(variable = varname, tiled = "", type = "zarr")
}

ds_ssebopeta <- climateR.catalogs::data_source$new(
    id   = "ssebopeta",
    pull = .pull_ssebopeta,
    tidy = .tidy_ssebopeta
)
