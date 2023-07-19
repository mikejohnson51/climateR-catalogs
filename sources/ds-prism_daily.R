.pull_prism_daily <- function(...) {
    arrow::as_arrow_table(climateR::read_dap_file(
        URL = "http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2021/PRISM_combo_20211010.nc#show=fetch",
        id  = "prism_daily"
    ))
}

# ---------------------------------------------------------------------

.tidy_prism_daily <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(duration = "1981-01-01/..", variable = varname) |>
        climateR::dap_meta() |>
        dplyr::mutate(
            type = "opendap",
            URL = gsub(pattern = "/2021\\/", replacement = "/{YYYY}/", URL) |>
                  gsub(pattern = "20211010", replacement = "{YYYYMMDD}"),
            tiled = "T",
            interval = "1 day"
        ) |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_prism_daily <- climateR.catalogs::data_source$new(
    id   = "prism_daily",
    pull = .pull_prism_daily,
    tidy = .tidy_prism_daily
)
