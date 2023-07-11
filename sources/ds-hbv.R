.pull_hbv <- function(...) {
    urls <- paste0(
        "/vsicurl/",
        "https://github.com/atsyplenkov/HBVr/releases/download/",
        "parameter-maps/correct_fold_",
        0:9,
        ".tiff"
    )

    v <- dplyr::tribble(
        ~variable, ~description,                             ~units,
        "beta",    "Shape coefficient of recharge function", NA,
        "FC",      "Maximum soil moisture storage",          "mm",
        "K0",      "Recession coefficient of upper zone",    "day-1",
        "K1",      "Recession coefficient of upper zone",    "day-1",
        "K2",      "Recession coefficient of lower zone",    "day-1",
        "LP",      "Soil moisture value above which actual evaporation reaches potential evaporation", NA,
        "PERC",    "Maximum percolation to lower zone",      "mm day-1",
        "UZL",     "Threshold parameter for extra outflow from upper zone", "mm",
        "TT",      "Threshold temperature",                  "C",
        "CFMAX",   "Degree-day factor",                      "mm \u00B0-11 day-1)",
        "CFR",     "Refreezing coefficient",                 NA,
        "CWH",     "Water holding capacity",                 NA
    )

    data <- list()

    for (i in seq_len(urls)) {
        r          <- terra::rast(urls[i])
        df         <- data.frame(variable = names(r))
        df$id      <- "HBV"
        df$varname <- df$variable
        df$asset   <- strsplit(basename(terra::sources(r)), "[.]")[[1]][1]
        df$URL     <- terra::sources(r)
        df$type    <- "multiband_tif"
        data[[i]]  <- dplyr::left_join(df, v, by = "variable")
    }

    arrow::as_arrow_table(dplyr::bind_rows(data))
}

.tidy_hbv <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        climateR.catalogs::vrt_meta() |>
        dplyr::mutate(type = "vrt", tiled = "") |>
        arrow::as_arrow_table()
}

ds_hbv <- climateR.catalogs::data_source$new(
    id   = "hbv",
    pull = .pull_hbv,
    tidy = .tidy_hbv
)
