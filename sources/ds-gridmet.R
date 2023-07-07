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

    out <- list()
    for (i in seq_len(nrow(.tbl))) {
        out[[i]] <- tryCatch({
            climateR::read_dap_file(
                URL     = .tbl$URL[i],
                id      = .tbl$variable[i],
                varmeta = TRUE
            )
        }, error = function(condition) NULL)
    }

    .tbl$URL <- NULL

    dplyr::bind_rows(out) |>
        dplyr::rename(variable = id) |>
        dplyr::left_join(.tbl, by = "variable") |>
        dplyr::mutate(tiled = "", type = "opendap") |>
        arrow::as_arrow_table()
}

#' GRIDMET Data Source
ds_gridmet <- climateR.catalogs::data_source$new(
    id   = "gridmet",
    pull = .pull_gridmet,
    tidy = .tidy_gridmet
)
