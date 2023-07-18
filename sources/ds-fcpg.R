.pull_fcpg <- function(...) {
    Sys.setenv(GDAL_SKIP = "DODS")

    base <- "https://prod-is-usgs-sb-prod-publish.s3.amazonaws.com/5f3ab12082ce8df5b6c4076a/params.json"
    info <- jsonlite::read_json(base, simplifyVector = TRUE)

    result <- data.frame(
        id = "USGS FCPG",
        varname = info$parameters,
        URL = unlist(info$urls)
    ) |>
        # dplyr::mutate(variable = gsub("_fcpg.vrt", "", basename(URL))) |>
        dplyr::mutate(variable = varname, URL = paste0("/vsicurl/", URL)) |>
        climateR.catalogs::vrt_meta() |>
        arrow::as_arrow_table()

    Sys.unsetenv("GDAL_SKIP")

    result
}

# ---------------------------------------------------------------------

.tidy_fcpg <- function(.tbl, ...) {
    #nolint start
    meta <- dplyr::tribble(
        ~variable, ~description, ~units,
        "lc1",     "Percent upstream Temperate or sub-polar needleleaf forest", "%",
        "lc2",     "Percent upstream Sub-polar taiga needleleaf forest", "%",
        "lc3",     "Percent upstream Tropical or sub-tropical broadleaf evergreen forest", "%",
        "lc4",     "Percent upstream Tropical or sub-tropical broadleaf deciduous forest", "%",
        "lc5",     "Percent upstream Temperate or sub-polar broadleaf deciduous forest", "%",
        "lc6",     "Percent upstream Mixed Forest", "%",
        "lc7",     "Percent upstream Tropical or subtropical shrubland", "%",
        "lc8",     "Percent upstream Temperate or sub-polar shrubland", "%",
        "lc9",     "Percent upstream Tropical or sub-tropical grassland", "%",
        "lc10",    "Percent upstream Temperate or sub-polar grassland", "%",
        "lc11",    "Percent upstream Sub-polar or polar shrubland-lichen-moss", "%",
        "lc12",    "Percent upstream Sub-polar or polar grassland-lichen-moss", "%",
        "lc13",    "Percent upstream Sub-polar or polar barren-lichen-moss", "%",
        "lc14",    "Percent upstream Wetland", "%",
        "lc15",    "Percent upstream Cropland", "%",
        "lc16",    "Barren Lands", "%",
        "lc17",    "Urban or Built-up", "%",
        "lc18",    "Water", "%",
        "lc19",    "Snow and Ice", "%",
        "prcp",    "Daymet Mean Annual Total Precipitation 1989 - 2018", "mm",
        "elev",    "Mean elevation of grid cell as estimated by the NHD DEM", "cm",
        "lat",     "latitude of the center of the pixel", "degrees",
        "tmin",    "Daymet Mean Daily Minimum Temperature 1989 - 2018", "C",
        "tmax",    "Daymet Mean Daily Maximum Temperature 1989 - 2018", "C",
        "slope",   "Slope of the land surface", "cm/cm",
        "str100",  "Binary raster approximating the presence of a stream", " ",
        "fac",     "An integer flow accumulation grid which contains the number of cells within CONUS draining to each cell within CONUS based on the HydroDEM", ""
    )
    #nolint end

    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(type = "vrt", tiled = "") |>
        dplyr::left_join(meta, by = "variable") |>
        climateR.catalogs::vrt_meta()
}

# ---------------------------------------------------------------------

ds_fcpg <- climateR.catalogs::data_source$new(
    id   = "fcpg",
    pull = .pull_fcpg,
    tidy = .tidy_fcpg
)
