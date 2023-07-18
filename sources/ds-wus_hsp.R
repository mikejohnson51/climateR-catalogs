.pull_wus_hsp <- function(...) {
    arrow::as_arrow_table(climateR::read_dap_file(
        URL = "https://cida.usgs.gov/thredds/dodsC/WUS_HSP/SD_A1B_2040s",
        id = "WUS_HSP"
    ))
}

# ---------------------------------------------------------------------

.tidy_wus_hsp <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        tidyr::separate_wider_delim(
            cols = "varname",
            names = c("junk", "scenario", "junk2", "model", "variable"),
            delim = "_",
            too_many = "merge",
            cols_remove = FALSE
        ) |>
        dplyr::mutate(tiled = "", type = "opendap") |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_wus_hsp <- climateR.catalogs::data_source$new(
    id   = "WUS_HSP",
    pull = .pull_wus_hsp,
    tidy = .tidy_wus_hsp
)
