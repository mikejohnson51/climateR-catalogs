.pull_nlcd <- function(...) {
    base     <- "https://storage.googleapis.com/feddata-r/nlcd/"
    year     <- c(2019, 2016, 2011, 2008, 2006, 2004, 2001)
    dataset  <- c("Land_Cover", "Impervious", "Tree_Canopy")
    landmass <- c("L48", "AK", "HI", "PR")

    df <-
        expand.grid(year, dataset, landmass, stringsAsFactors = FALSE) |>
        setNames(c("year", "dataset", "landmass")) |>
        tidyr::unite(
            col = "filename",
            c(year, dataset, landmass),
            remove = FALSE
        ) |>
        dplyr::mutate(
            ds_sp       = gsub("_", " ", dataset),
            URL         = paste0(base, filename, ".tif"),
            asset       = paste(year, ds_sp, landmass),
            varname     = dataset,
            variable    = dataset,
            description = paste("USGS NLCD", ds_sp, year, landmass),
            units = dplyr::case_when(
                varname == "Land_Cover" ~ "",
                TRUE                    ~ "%"
            )
        ) |>
        dplyr::select(-c(year, dataset, landmass, filename, ds_sp)) |>
        dplyr::rowwise() |>
        dplyr::mutate(exists = !httr::http_error(URL)) |>
        dplyr::ungroup() |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

.tidy_nlcd <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::filter(exists) |>
        dplyr::mutate(
            URL   = paste0("/vsicurl/", URL),
            id    = "NLCD",
            type  = "vrt",
            tiled = ""
        ) |>
        dplyr::select(-exists) |>
        climateR.catalogs::vrt_meta() |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_nlcd <- climateR.catalogs::data_source$new(
    id   = "nlcd",
    pull = .pull_nlcd,
    tidy = .tidy_nlcd
)
