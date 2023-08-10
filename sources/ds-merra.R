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

        g = data.frame(
            asset = assets[a],
            date = seq.Date(as.Date("1980-01-01"), Sys.Date(), by = "d") ) |>
        dplyr::mutate(
            month = format(date, "%m"),
            year = format(date, "%Y"),
            ext = paste0(base, asset, "/", year, "/", month)) |>
        dplyr::distinct(ext, asset)

        x = climateR.catalogs::read_tds(g$ext[1],
                                        id = g$asset[1],
                                        append = "") %>%
            dplyr::filter(grepl(".nc4$", URL)) %>%
            mutate(year = g$year[1], month = g$month[1],
                   day = sprintf("%02s", 1:n()),
                   date = as.Date(paste(year, month, day, sep = "-"))) %>%
            mutate(link = gsub(paste0("/", year[1], "/"), "/{YYYY}/", link),
                   link = gsub(paste0("/",month[1], "/"), "/{MM}/", link),
                   link = gsub(paste0(year[1], month[1], day[1]), "{YYYYMMDD}", link),
                   minDate = min(date),
                   maxDate = max(date)
                   ) %>%
            slice(1)

        x =  x |>
            dplyr::mutate(
                URL = paste0(
                    "https://goldsmr5.gesdisc.eosdis.nasa.gov/opendap/hyrax/",
                    link
                )
            )

        var = climateR::read_dap_file(
            glue(x$URL, YYYY = g$year[1], MM = g$month[1], YYYYMMDD = paste0(g$year[1], g$month[1], g$day[1])),
            id = "MERRA2")

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

    xx = data.table::rbindlist(merra1)





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
