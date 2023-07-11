.pull_bcca <- function(...) {
    arrow::as_arrow_table(dplyr::bind_rows(
        climateR::read_dap_file(
            URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future",
            id  = "bcca"
        ),
        climateR::read_dap_file(
            URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical",
            id  = "bcca"
        )
    ))
}

.tidy_bcca <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        tidyr::separate_wider_delim(
            cols  = "varname",
            names = c(NA, NA, "variable", NA, "model", "scenario", "ensemble"),
            delim = "_",
            cols_remove = FALSE
        ) |>
        dplyr::mutate(tiled = "T", type = "opendap") |>
        arrow::as_arrow_table()
}

ds_bcca <- climateR.catalogs::data_source$new(
    id   = "bcca",
    pull = .pull_bcca,
    tidy = .tidy_bcca
)
