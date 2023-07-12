.pull_isric <- function(...) {
    base <- "https://files.isric.org/soilgrids/latest/data"

    ids <- base |>
           rvest::read_html() |>
           rvest::html_nodes("a") |>
           rvest::html_attr("href") |>
           grep(pattern = "/$", value = TRUE) |>
           gsub(pattern = "^.", replacement = base)

    out <- list()

    for (i in seq_along(ids)) {
        tmp <- rvest::read_html(ids[i]) |>
               rvest::html_nodes("a") |>
               rvest::html_attr("href") |>
               grep(pattern = ".vrt$", value = TRUE)

        out[[i]] <- glue::glue("/vsicurl/{ids[i]}{tmp}")
    }

    arrow::arrow_table(URL = unlist(out), id = "ISRIC Soil Grids")
}

# ---------------------------------------------------------------------

.tidy_isric <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(varname = gsub(".vrt", "", basename(URL))) |>
        tidyr::separate(
            col = "varname",
            c("variable", "depth", "measure"),
            sep = "_",
            extra = "merge",
            remove = FALSE
        ) |>
        dplyr::mutate(
            description = paste(measure, varialle, depth),
            tiled = "NA",
            type = "vrt",
            variable = varname
        ) |>
        climateR.catalogs::vrt_meta(all = FALSE) |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_isric <- climateR.catalogs::data_source$new(
    id   = "isric",
    pull = .pull_isric,
    tidy = .tidy_isric
)
