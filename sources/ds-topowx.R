.pull_topowx <- function(...) {
    arrow::as_arrow_table(dplyr::bind_rows(
        climateR::read_dap_file(
            URL = "https://cida.usgs.gov/thredds/dodsC/topowx",
            id = "topowx_daily"
        ),
        climateR::read_dap_file(
            URL = "https://cida.usgs.gov/thredds/dodsC/topowx_monthly",
            id = "topowx_monthly"
        ),
        climateR::read_dap_file(
            URL = "https://cida.usgs.gov/thredds/dodsC/topowx_normals",
            id = "topowx_normals"
        )
    ))
}

.tidy_topowx <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(tiled = "", variable = varname, type = "opendap") |>
        arrow::as_arrow_table()
}

ds_topowx <- climateR.catalogs::data_source$new(
    id   = "topowx",
    pull = .pull_topowx,
    tidy = .tidy_topowx
)