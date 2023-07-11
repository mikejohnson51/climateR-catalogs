#' @keywords
.pull_daymet4 <- function(...) {
    climateR.catalogs::read_tds(
        paste0(
            "https://thredds.daac.ornl.gov",
            "/thredds/catalog/daymet-v4-agg/catalog.html"
        ),
        "daymet4"
    ) |>
        dplyr::mutate(
            URL = paste0(
                "https://thredds.daac.ornl.gov/thredds/dodsC/",
                link
            )
        ) |>
        dplyr::filter(grepl("ncml", URL)) |>
        arrow::as_arrow_table()
}

#' @keywords internal
.tidy_daymet4 <- function(.tbl, ...) {
    .tbl <- dplyr::collect(.tbl)
    out  <- list()

    for (i in nrow(.tbl)) {
        out[[i]] <- ncmeta::nc_vars(.tbl$URL[i]) |>
                    dplyr::filter(ndims == 3) |>
                    dplyr::select(variable = name) |>
                    dplyr::mutate(
                        URL = .tbl$URL[i],
                        id  = "daymet4",
                        model = gsub(".ncml", "", basename(URL))
                    ) |>
                    climateR::dap_meta() |>
                    dplyr::mutate(
                        tiled    = "",
                        variable = varname,
                        type     = "opendap"
                    )
    }

    dplyr::bind_rows(out) |>
        dplyr::distinct() |>
        arrow::as_arrow_table()
}

#' Daymet Version 4 Data Source
ds_daymet4 <- climateR.catalogs::data_source$new(
    id   = "daymet4",
    pull = .pull_daymet4,
    tidy = .tidy_daymet4
)
