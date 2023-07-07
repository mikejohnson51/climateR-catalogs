.pull_bcsd <- function(...) {
    arrow::as_arrow_table(climateR::read_dap_file(
        URL = "https://cida.usgs.gov/thredds/dodsC/bcsd_obs",
        id  = "bcsd"
    ))
}

.tidy_bcsd <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::rename(variable = varname) |>
        climateR::dap_meta() |>
        dplyr::mutate(tiled = "", type = "opendap")
}

ds_bcsd <- climateR.catalogs::data_source$new(
    id   = "bcsd",
    pull = .pull_bcsd,
    tidy = .tidy_bcsd
)