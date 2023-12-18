.pull_livneh_monthly <- function(...) {
    dates <- data.frame(
        dates = seq.Date(
            as.Date("1950-01-01"),
            as.Date("2013-12-31"),
            by = "day"
        )
    ) |>
        dplyr::mutate(
            ym = format(dates, "%Y%m"),
            y =  format(dates, "%Y")
        )

    tmp <- dplyr::group_by(dates, ym) |>
           dplyr::slice(1)

    urls <- glue::glue(
        "https://www.ncei.noaa.gov",
        "/thredds-ocean/dodsC/ncei/archive/data/0129374/monthly/",
        "livneh_NAmerExt_15Oct2014.{tmp$ym}.mon.nc"
    )

    .tbl = climateR::read_dap_file(urls[2], varname = NULL, id = "Livneh_monthly") |>
        arrow::as_arrow_table()

    return(.tbl)
}

# ---------------------------------------------------------------------

.tidy_livneh_monthly <- function(.tbl, ...) {
    dates <- data.frame(
        dates = seq.Date(
            as.Date("1950-01-01"),
            as.Date("2013-12-31"),
            by = "day"
        )
    ) |>
        dplyr::mutate(
            ym = format(dates, "%Y%m"),
            y =  format(dates, "%Y")
        )

    tmp <- dplyr::group_by(dates, ym) |>
           dplyr::slice(1)

    df1 <- data.frame(
        id = "Livneh_monthly",
        type = "opendap",
        ym = tmp$ym,
        tiled = "T",
        URL = glue::glue(
            "https://www.ncei.noaa.gov",
            "/thredds-ocean/dodsC/ncei/archive/data/0129374/monthly",
            "/livneh_NAmerExt_15Oct2014.{tmp$ym}.mon.nc"
        )
    )

    df2 <- dates |>
           dplyr::group_by(ym) |>
           dplyr::mutate(
               duration = paste0(dates[1], "/", dates[dplyr::n()]),
               nT = 1
            ) |>
            dplyr::slice(1) |>
            dplyr::select(ym, duration, nT)

    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(URL = NULL) |>
        dplyr::right_join(df1, by = "id") |>
        dplyr::mutate(
            duration = NULL,
            interval = "1 month",
            nT = NULL,
            variable = varname
        ) |>
        dplyr::left_join(df2, by = "ym") |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_livneh_monthly <- climateR.catalogs::data_source$new(
    id   = "Livneh_monthly",
    pull = .pull_livneh_monthly,
    tidy = .tidy_livneh_monthly
)
