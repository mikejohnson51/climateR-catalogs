#' @keywords internal
.pull_terraclim_normals <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_tds(
        paste0(
            "http://thredds.northwestknowledge.net:8080",
            "/thredds/catalog/TERRACLIMATE_ALL/summaries/catalog.html"
        ),
        "terraclim_normals",
        ""
    ))
}

#' @keywords internal
.tidy_terraclim_normals <- function(.tbl, ...) {
    dplyr::collect(.tbl) |>
        dplyr::mutate(
            URL = paste0("http://thredds.northwestknowledge.net:8080",
                         "/thredds/dodsC/TERRACLIMATE_ALL/summaries/",
                         basename(link)),
            link2 = gsub(".nc", "", basename(link))
        ) |>
        dplyr::filter(link2 != "summaries") |>
        tidyr::separate_wider_delim(
            cols  = "link2",
            names = c("scenario", "variable"),
            delim = "_"
        ) |>
        dplyr::mutate(scenario = gsub("TerraClimate", "", scenario)) |>
        dplyr::filter(!is.na(variable)) |>
        climateR::dap_meta() |>
        dplyr::mutate(
            tiled    = "",
            interval = "1 month",
            type     = "opendap"
        ) |>
        arrow::as_arrow_table()
}

#' TerraClimate Normals Data Source
ds_terraclim_normals <- climateR.catalogs::data_source$new(
    id   = "terraclim_normals",
    pull = .pull_terraclim_normals,
    tidy = .tidy_terraclim_normals
)