.pull_livneh_daily <- function(...) {
    dates <- seq.Date(
        as.Date("1950-01-01"),
        as.Date("2013-12-31"),
        by = "day"
    )

    urls <- glue::glue(
        "https://www.ncei.noaa.gov",
        "/thredds-ocean/dodsC/ncei/archive/data/0129374/daily/",
        "livneh_NAmerExt_15Oct2014.{unique(format(dates, '%Y%m'))}.nc"
    )

    lapply(
        urls,
        climateR::read_dap_file,
        id = "Livneh_daily",
        varname = NULL
    ) |>
        dplyr::bind_rows() |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

.tidy_livneh_daily <- function(.tbl, ...) {
    dates <- seq.Date(
        as.Date("1950-01-01"),
        as.Date("2013-12-31"),
        by = "day"
    )

    df1 <-
        data.frame(
            id = "Livneh_daily",
            URL = glue::glue(
                "https://www.ncei.noaa.gov",
                "/thredds-ocean/dodsC/ncei/archive/data/0129374/daily/",
                "livneh_NAmerExt_15Oct2014.{unique(format(dates, '%Y%m'))}.nc"
            ),
            type = "opendap",
            ym = unique(format(dates, "%Y%m")),
            tiled = "T"
        )

    df2 <-
        data.frame(date = dates, ym = format(dates, "%Y%m")) |>
        dplyr::group_by(ym) |>
        dplyr::mutate(
            duration = paste0(date[1], "/", date[dplyr::n()]),
            nT = dplyr::n()
        ) |>
        dplyr::slice(1) |>
        dplyr::select(ym, duration, nT)

    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(URL = NULL) |>
        dplyr::right_join(df1, by = "id") |>
        dplyr::mutate(duration = NULL, nT = NULL, variable = varname) |>
        dplyr::left_join(df2, by = "ym") |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_livneh_daily <- climateR.catalogs::data_source$new(
    id   = "Livneh_daily",
    pull = .pull_livneh_daily,
    tidy = .tidy_livneh_daily
)
