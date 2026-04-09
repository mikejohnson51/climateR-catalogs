#' @keywords internal
.pull_terraclim_counterfactual <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_tds(
        paste0(
            "http://thredds.northwestknowledge.net:8080",
            "/thredds/catalog/TERRACLIMATE_ALL/counterfactual/catalog.html"
        ),
        "terraclim_counterfactual",
        ""
    ))
}

#' @keywords internal
.tidy_terraclim_counterfactual <- function(.tbl, ...) {
    dplyr::collect(.tbl) |>
        dplyr::mutate(
            URL = paste0("http://thredds.northwestknowledge.net:8080",
                         "/thredds/dodsC/",
                         gsub("_SCAN", "", link)),
            link2 = gsub("\\.nc$", "", basename(link))
        ) |>
        dplyr::filter(grepl("^TerraClimate_counterfactual_", link2)) |>
        tidyr::separate_wider_delim(
            cols  = "link2",
            names = c(NA, NA, "variable", NA),
            delim = "_"
        ) |>
        dplyr::mutate(scenario = "counterfactual") |>
        climateR::dap_meta() |>
        dplyr::mutate(
            tiled = "",
            type  = "opendap"
        ) |>
        arrow::as_arrow_table()
}

#' TerraClimate Counterfactual Data Source
ds_terraclim_counterfactual <- climateR.catalogs::data_source$new(
    id   = "terraclim_counterfactual",
    pull = .pull_terraclim_counterfactual,
    tidy = .tidy_terraclim_counterfactual
)
