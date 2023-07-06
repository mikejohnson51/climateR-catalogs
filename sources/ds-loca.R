.pull_loca <- function(...) {
    arrow::as_arrow_table(plyr::bind_rows(
        climateR::read_dap_file(
            "https://cida.usgs.gov/thredds/dodsC/loca_historical",
            id = "loca"
        ),
        climateR::read_dap_file(
            "https://cida.usgs.gov/thredds/dodsC/loca_future",
            id = "loca"
        )
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
        dplyr::mutate(tiled = "T", type = "opendap") |>
        arrow::as_arrow_table()
}

ds_loca <- climateR.catalogs::data_source$new(
    id   = "loca",
    pull = .pull_loca,
    tidy = .tidy_loca
)