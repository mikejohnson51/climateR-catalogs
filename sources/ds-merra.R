.pull_merra <- function(...) {
    base <- "https://goldsmr5.gesdisc.eosdis.nasa.gov/opendap/MERRA2/"

    das <- rvest::read_html(base) |>
           rvest::html_nodes("a")
    URL <- rvest::html_attr(das, "href") |>
           gsub(pattern = "\\.[a-z]*$", replacement = "") |>
           unique()

    assets <- dirname(URL[grepl("/contents", URL)])

    merra1 <- list()
    for (a in seq_along(assets)) {
        g = expand.grid(
            asset = assets[a],
            year = 1980:2023,
            month = stringr::str_pad(1:12, 2, side = "left", pad = "0")
        ) |>
        dplyr::mutate(ext = paste0(base, asset, "/", year, "/", month))

        x = climateR.catalogs::read_tds(g$ext[1], id = g$Var1[1], append = "")

        x = dplyr::filter(x, grepl(".nc4$", x$URL)) |>
            dplyr::mutate(
                URL = paste0(
                    "https://goldsmr5.gesdisc.eosdis.nasa.gov/opendap",
                    link
                )
            )

        var = climateR::read_dap_file(x$URL[1], id = "MERRA2")

        merra = list()

        for (i in seq_len(nrow(g))){
            dates = paste(g$year[i], g$month[i], "01", sep = "-") |>
                    as.Date()

            files = gsub("19840101", "{date}", unique(basename(var$URL)))

            x2 = data.frame(
                date = seq.Date(
                    dates,
                    by = "day",
                    length.out = lubridate::days_in_month(dates)
                )
            ) |>
                dplyr::mutate(
                    date2 = gsub("-", "", date),
                    duration = glue::glue("{date} 01:30:00/{date} 22:30:00"),
                    URL = glue::glue(files, date = date2),
                    tiled = "T"
                )

            merra[[i]] = tidyr::crossing(
                dplyr::select(x2, -date, -date2),
                dplyr::select(var, -URL, -duration)
            ) |>
                dplyr::mutate(URL = paste0(dirname(g$ext[i]), URL))
        }

        merra1[[a]] = data.table::rbindlist(merra) |>
            dplyr::mutate(variable  = varname, type = "opendap") |>
            dplyr::mutate(asset = assets[a])

        message(
            "\tFinished [",
            assets[a],
            "] (", a, " of ",
            length(assets), ")"
        )
    }

    data.table::rbindlist(merra1) |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

# nullopt
.tidy_merra <- function(.tbl, ...) {
    .tbl
}

# ---------------------------------------------------------------------

ds_merra <- climateR.catalogs::data_source$new(
    id   = "merra",
    pull = .pull_merra,
    tidy = .tidy_merra
)
