.pull_prism_monthly <- function(...) {
    arrow::as_arrow_table(climateR::read_dap_file(
        URL = "https://cida.usgs.gov/thredds/dodsC/prism_v2",
        id  = "prism_monthly"
    ))
}

.tidy_prism_monthly <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(variable = varname, tiled = "", type = "opendap") |>
        arrow::as_arrow_table()
}

ds_prism_monthly <- climateR.catalogs::data_source$new(
    id = "prism_monthly",
    pull = .pull_prism_monthly,
    tidy = .tidy_prism_monthly
)