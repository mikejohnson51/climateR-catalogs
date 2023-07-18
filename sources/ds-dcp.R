.pull_dcp <- function(...) {
    arrow::as_arrow_table(climateR::read_dap_file(
        URL = "https://cida.usgs.gov/thredds/dodsC/dcp/conus_t",
        id  = "dcp"
    ))
}

.tidy_dcp <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        tidyr::separate_wider_delim(
            cols        = "varname",
            names       = c("model", "scenario", "variable", NA, NA),
            delim       = "_",
            too_few     = "align_end",
            cols_remove = FALSE
        ) |>
        dplyr::mutate(variable = varname, tiled = "", type = "opendap")
}

ds_dcp <- climateR.catalogs::data_source$new(
    id   = "dcp",
    pull = .pull_dcp,
    tidy = .tidy_dcp
)
