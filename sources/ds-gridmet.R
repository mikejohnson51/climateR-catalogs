#' @keywords internal
.pull_gridmet <- function(...) {
    paste0("http://thredds.northwestknowledge.net:8080",
           "/thredds/reacch_climate_MET_aggregated_catalog.html") |>
        climateR.catalogs::read_tds(id = "gridmet") |>
        tidyr::separate_wider_delim(
            cols  = "link",
            names = c(NA, NA, "variable", NA, NA, NA),
            delim = "_"
        ) |>
        arrow::as_arrow_table()
}

#' @keywords internal
.tidy_gridmet <- function(.tbl, ...) {
    .tbl <- dplyr::collect(.tbl)

    dplyr::rowwise(.tbl) |>
        dplyr::group_map(~ tryCatch({
            climateR::read_dap_file(
                URL     = .x$URL,
                id      = .x$variable,
                varmeta = TRUE
            )}, error = function(condition) NULL)
        ) |>
        dplyr::bind_rows() |>
        dplyr::rename(variable = id) |>
        dplyr::left_join(dplyr::select(.tbl, -URL), by = "variable") |>
        dplyr::mutate(tiled = "", type = "opendap") |>
        arrow::as_arrow_table()
}

#' GRIDMET Data Source
ds_gridmet <- climateR.catalogs::data_source$new(
    id   = "gridmet",
    pull = .pull_gridmet,
    tidy = .tidy_gridmet
)
