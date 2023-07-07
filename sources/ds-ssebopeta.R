.pull_ssebopeta <- function(...) {
    arrow::as_arrow_table(climateR::read_dap_file(
        URL = "https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly",
        id = "ssebopeta"
    ))
}

.tidy_ssebopeta <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        climateR::dap_meta() |>
        dplyr::mutate(tiled = "", type = "opendap")
}

ds_ssebopeta <- climateR.catalogs::data_source$new(
    id   = "ssebopeta",
    pull = .pull_ssebopeta,
    tidy = .tidy_ssebopeta
)