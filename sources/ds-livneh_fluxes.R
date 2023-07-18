.pull_livneh_fluxes <- function(...) {
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
        "/thredds-ocean/dodsC/livnehmodel/{tmp$y}/",
        "Fluxes_Livneh_NAmerExt_15Oct2014.{tmp$ym}.nc"
    )

    climateR::read_dap_file(urls[2], varname = NULL, id = "Livneh_fluxes") |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

.tidy_livneh_fluxes <- function(.tbl, ...) {
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
        id = "Livneh_fluxes",
        type = "opendap",
        ym = tmp$ym,
        tiled = "T",
        URL = glue::glue(
            "https://www.ncei.noaa.gov",
            "/thredds-ocean/dodsC/livnehmodel/{tmp$y}/",
            "Fluxes_Livneh_NAmerExt_15Oct2014.{tmp$ym}.nc"
        )
    )

    df2 <- dates |>
           dplyr::group_by(ym) |>
           dplyr::mutate(
               duration = paste0(dates[1], "/", dates[dplyr::n()]),
               nT = dplyr::n()
            ) |>
            dplyr::slice(1) |>
            dplyr::select(ym, duration, nT)

    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(URL = NULL) |>
        dplyr::right_join(df1, by = "id") |>
        dplyr::mutate(
            duration = NULL,
            nT = NULL,
            variable = varname
        ) |>
        dplyr::left_join(df2, by = "ym") |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_livneh_fluxes <- climateR.catalogs::data_source$new(
    id   = "Livneh_fluxes",
    pull = .pull_livneh_fluxes,
    tidy = .tidy_livneh_fluxes
)
