.pull_maurer <- function(...) {
    arrow::as_arrow_table(climateR::read_dap_file(
        URL = "https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml",
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
        dplyr::mutate(tiled = "", type = "opendap")
}

ds_maurer <- climateR.catalogs::data_source$new(
    id   = "maurer",
    pull = .pull_maurer,
    tidy = .tidy_maurer
)