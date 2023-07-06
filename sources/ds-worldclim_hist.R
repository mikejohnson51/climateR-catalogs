#' @keywords internal
.pull_worldclim_hist <- function(...) {
    df <- data.frame(
        source   = "WorldClim2.1",
        tiled    = "T",
        variable = c(
            "tmin",
            "tmax",
            "tavg",
            "prec",
            "srad",
            "wind",
            "vapr"
        ),
        description = c(
            "minimum temperature",
            "maximum temperature",
            "average temperature",
            "precipitation ",
            "solar radiation ",
            "wind speed",
            "water vapor pressure"
        ),
        units = c(
            "C",
            "C",
            "C",
            "mm",
            "(kJ m-2 day-1",
            "m s-1",
            "kPa"
        ),
        duration = "1970-01-01/2000-01-01",
        interval = "monthly",
        nT       = 12
    ) |>
    dplyr::slice(rep(1:7, each = 12)) |>
    dplyr::mutate(month = rep(1:7, each = 12), varname = variable)

    df1 <- dplyr::mutate(df, id = "wc2.1_10m")
    df2 <- dplyr::mutate(df, id = "wc2.1_5m")
    df3 <- dplyr::mutate(df, id = "wc2.1_2.5m")
    df4 <- dplyr::mutate(df, id = "wc2.1_30s")

    dplyr::bind_rows(df1, df2, df3, df4) |>
        dplyr::mutate(URL = paste0(
            "/vsizip/{/vsicurl/",
            "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/",
            id, "_", varname, ".zip}/",
            id, "_", gsub("BIO", "", varname), "_",
            sprintf("%02d", month), ".tif"
        )) |>
        arrow::as_arrow_table()
}

#' @keywords internal
.tidy_worldclim_hist <- function(.tbl, ...) {
    dep <- dplyr::group_by(.tbl, id) |>
           dplyr::slice(1) |>
           dplyr::select(URL, id) |>
           dplyr::ungroup() |>
           dplyr::mutate(
                   X1 = NA,    Xn = NA,
                   Y1 = NA,    Yn = NA,
                 resX = NA,  resY = NA,
                ncols = NA, nrows = NA, crs = NA
           )

    for (i in seq_len(nrow(dep))) {
        for (t in 1:10) {
            try({
                r <- terra::rast(dep$URL[i])
                break
            }, silent = TRUE)
        }

        dep$X1[i]    <- terra::xmin(r)
        dep$Xn[i]    <- terra::xmax(r)
        dep$Y1[i]    <- terra::xmin(r)
        dep$Yn[i]    <- terra::ymax(r)
        dep$resX[i]  <- terra::xres(r)
        dep$resY[i]  <- terra::yres(r)
        dep$ncols[i] <- terra::ncol(r)
        dep$nrows[i] <- terra::nrow(r)
        dep$crs[i]   <- sf::st_crs(r)$proj4string
    }

    dplyr::left_join(.tbl, dplyr::select(dep, -URL), by = "id") |>
        dplyr::mutate(type = "VRT", toptobottom = FALSE) |>
        arrow::as_arrow_table()
}

ds_worldclim_hist <- climateR.catalogs::data_source$new(
    id   = "worldclim_hist",
    pull = .pull_worldclim_hist,
    tidy = .tidy_worldclim_hist
)