.pull_merra <- function(...) {
    process_merra = function(base) {
        URL <- rvest::read_html(base) |>
            rvest::html_nodes("a") |>
            rvest::html_attr("href") |>
            gsub(pattern = "\\.[a-z]*$", replacement = "") |>
            unique()

        assets <- dirname(URL[grepl("/contents", URL)])

        merra <- list()

        for (a in seq_along(assets)) {
            x = climateR.catalogs::read_tds(paste0(base, assets[a], "/1980/01"),
                                            id = assets[a],
                                            append = "") %>%
                dplyr::filter(grepl(".nc4$", URL)) %>%
                dplyr::mutate(URL = paste0(gsub("MERRA2", "hyrax", base), link))

            merra[[a]] = climateR::read_dap_file(x$URL[1], id = "MERRA2") %>%
                dplyr::mutate(
                    asset = assets[a],
                    duration = "1981-01-01 00:00:00/..",
                    variable = varname,
                    type = "opendap",
                    URL = gsub(pattern = "/1980\\/", replacement = "/{YYYY}/", URL) |>
                        gsub(pattern = "/01\\/", replacement = "/{MM}/", URL) |>
                        gsub(pattern = "19800101", replacement = "{YYYYMMDD}"),
                    tiled = "T"
                )
        }

        dplyr::bind_rows(merra)
    }

    g5 = process_merra(base = "https://goldsmr5.gesdisc.eosdis.nasa.gov/opendap/MERRA2/")
    g4 = process_merra(base = "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/")

    arrow::as_arrow_table(dplyr::bind_rows(g4, g5))
}

# ---------------------------------------------------------------------

# nullopt
.tidy_merra <- function(.tbl, ...) {
    .tbl
}

# ---------------------------------------------------------------------

ds_merra <- climateR.catalogs::data_source$new(
    id   = "MERRA2",
    pull = .pull_merra,
    tidy = .tidy_merra
)
