#' @keywords internal
.pull_terraclim <- function(...) {
    .tbl = arrow::as_arrow_table(climateR.catalogs::read_tds(
        paste0(
            "http://thredds.northwestknowledge.net:8080",
            "/thredds/terraclimate_aggregated.html"
        ),
        "terraclim"
    ))

    return(.tbl)
}

#' @keywords internal
.tidy_terraclim <- function(.tbl, ...) {
   .tbl = dplyr::collect(.tbl) |>
         dplyr::mutate(link = gsub("-", "_", link),
                      URL = gsub("-", "_", URL)) |>
         tidyr::separate_wider_delim(
            cols    = "link",
            names   = c(NA, NA, "variable", NA, NA, NA),
            delim   = "_",
            too_few = "align_end"
        ) |>
        climateR::variable_meta() |>
        climateR::dap_meta() |>
        dplyr::mutate(tiled = "", type = "opendap") |>
        arrow::as_arrow_table()

   return(.tbl)
}

#' TerraClimate Data Source
ds_terraclim <- climateR.catalogs::data_source$new(
    id   = "terraclim",
    pull = .pull_terraclim,
    tidy = .tidy_terraclim
)
