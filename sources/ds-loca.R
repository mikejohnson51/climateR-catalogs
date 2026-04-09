.pull_loca <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_children(
        parent_url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/loca",
        id = "loca"
    ))
}

.tidy_loca <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        tidyr::separate_wider_delim(
            cols        = "varname",
            names       = c("variable", "model", "ensemble", "scenario"),
            delim       = "_",
            cols_remove = FALSE
        ) |>
        dplyr::mutate(tiled = "T", type = "zarr") |>
        arrow::as_arrow_table()
}

ds_loca <- climateR.catalogs::data_source$new(
    id   = "loca",
    pull = .pull_loca,
    tidy = .tidy_loca
)
