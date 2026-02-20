#' @keywords internal
.pull_terraclim_4c <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_tds(
        paste0(
            "http://thredds.northwestknowledge.net:8080",
            "/thredds/catalog/TERRACLIMATE_ALL/data_plus4C/catalog.html"
        ),
        "terraclim_4c",
        ""
    ))
}

#' @keywords internal
.tidy_terraclim_4c <- function(.tbl, ...) {
    dplyr::collect(.tbl) |>
        dplyr::mutate(
            URL = paste0("http://thredds.northwestknowledge.net:8080",
                         "/thredds/dodsC/",
                         gsub("_SCAN", "", link)),
            link2 = gsub("\\.nc$", "", basename(link))
        ) |>
        dplyr::filter(grepl("^TerraClimate_plus4C_", link2)) |>
        tidyr::separate_wider_delim(
            cols  = "link2",
            names = c(NA, NA, "variable", NA),
            delim = "_"
        ) |>
        dplyr::mutate(scenario = "plus4C") |>
        climateR::dap_meta() |>
        dplyr::mutate(
            tiled = "",
            type  = "opendap"
        ) |>
        arrow::as_arrow_table()
}

#' TerraClimate +4C Scenario Data Source
ds_terraclim_4c <- climateR.catalogs::data_source$new(
    id   = "terraclim_4c",
    pull = .pull_terraclim_4c,
    tidy = .tidy_terraclim_4c
)
