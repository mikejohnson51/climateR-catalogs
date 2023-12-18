#' @keywords internal
.pull_ldas <- function(...) {
   .tbl =  rvest::read_html("https://hydro1.gesdisc.eosdis.nasa.gov/dods/") |>
        rvest::html_nodes("a") |>
        rvest::html_attr("href") |>
        gsub(pattern = "\\.[a-z]*$", replacement = "") |>
        unique() |>
        Filter(f = function(url) grepl("NLDAS|GLDAS", url)) |>
        lapply(FUN = function(url) {
            climateR::read_dap_file(paste0(url, "/"), id = basename(url))
        }) |>
        dplyr::bind_rows() |>
        arrow::as_arrow_table()

   return(.tbl)
}

#' @keywords internal
.tidy_ldas <- function(.tbl, ...) {
    .tbl = dplyr::collect(.tbl) |>
        dplyr::mutate(
            variable = varname,
            tiled    = "",
            type     = "opendap"
        ) |>
        tidyr::separate_wider_delim(
            cols     = "id",
            names    = c("id", "model"),
            delim    = "_",
            too_many = "merge"
        ) |>
        arrow::as_arrow_table()

    return(.tbl)
}

#' LDAS Data Source
ds_ldas <- climateR.catalogs::data_source$new(
    id   = "ldas",
    pull = .pull_ldas,
    tidy = .tidy_ldas
)
