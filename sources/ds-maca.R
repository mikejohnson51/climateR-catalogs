#' @keywords internal
.pull_maca <- function(...) {
    dplyr::bind_rows(
        climateR.catalogs::read_tds(
            URL = paste0(
                "http://thredds.northwestknowledge.net:8080/thredds/",
                "reacch_climate_CMIP5_aggregated_macav2_catalog.html"
            ),
            id = "maca_day"
        ),
        climateR.catalogs::read_tds(
            URL = paste0(
                "http://thredds.northwestknowledge.net:8080/thredds/",
                "reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html"
            ),
            id = "maca_month"
        )
    ) |>
    tidyr::separate_wider_delim(
        cols = "link",
        names = c(
            rep(NA, 2),
            "variable", "model", "ensemble", "scenario",
            rep(NA, 4)
        ),
        delim = "_"
    )
}

#' @keywords internal
.tidy_maca <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
    climateR::dap_meta() |>
    dplyr::mutate(
        tiled       = "T",
        type        = "opendap"
    ) |>
    arrow::as_arrow_table()
}

#' MACA Data Source
ds_maca <- climateR.catalogs::data_source$new(
    id   = "maca",
    pull = .pull_maca,
    tidy = .tidy_maca
)
