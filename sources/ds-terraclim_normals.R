#' @keywords internal
.pull_terraclim_normals <- function(...) {
    climateR.catalogs::read_tds(
        paste0(
            "http://thredds.northwestknowledge.net:8080",
            "/thredds/catalog/TERRACLIMATE_ALL/summaries/catalog.html"
        ),
        "terraclim_normals",
        ""
    )
}

#' @keywords internal
.tidy_terraclim_normals <- function(.tbl, ...) {
    dplyr::collect(.tbl) |>
        dplyr::mutate(
            URL = paste0("http://thredds.northwestknowledge.net:8080",
                         "/thredds/dodsC/TERRACLIMATE_ALL/summaries/",
                         basename(.data$link)),
            link2 = gsub(".nc", "", basename(.data$link))
        ) |>
        dplyr::filter(.data$link2 != "summaries") |>
        tidyr::separate_wider_delim(
            cols  = "link2",
            names = c("scenario", "variable"),
            delim = "_"
        ) |>
        dplyr::mutate(scenario = gsub("TerraClimate", "", .data$scenario)) |>
        dplyr::filter(!is.na(.data$variable)) |>
        climateR::dap_meta() |>
        dplyr::mutate(
            tiled    = "",
            interval = "1 month",
            type     = "opendap"
        )
}

#' TerraClimate Normals Data Source
ds_terraclim_normals <- climateR.catalogs::data_source$new(
    id   = "terraclim_normals",
    pull = .pull_terraclim_normals,
    tidy = .tidy_terraclim_normals
)