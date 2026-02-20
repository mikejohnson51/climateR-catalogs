.pull_maurer <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_collection(
        url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/maurer",
        id  = "maurer"
    ))
}

.tidy_maurer <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        tidyr::separate_wider_delim(
            cols = "varname",
            names = c("scenario", "model", "ensemble", "variable"),
            delim = "_",
            too_many = "merge",
            cols_remove = FALSE
        ) |>
        dplyr::mutate(tiled = "", type = "zarr")
}

ds_maurer <- climateR.catalogs::data_source$new(
    id   = "maurer",
    pull = .pull_maurer,
    tidy = .tidy_maurer
)
