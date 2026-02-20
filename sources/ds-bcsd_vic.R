.pull_bcsd_vic <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_collection(
        url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/bcsd_mon_vic",
        id  = "bcsd_vic"
    ))
}

.tidy_bcsd_vic <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        tidyr::separate_wider_delim(
            cols        = "varname",
            names       = c("model", "scenario", "ensemble", "variable"),
            delim       = "_",
            too_many    = "merge",
            cols_remove = FALSE
        ) |>
        dplyr::mutate(tiled = "", type = "zarr") |>
        arrow::as_arrow_table()
}

ds_bcsd_vic <- climateR.catalogs::data_source$new(
    id   = "bcsd_vic",
    pull = .pull_bcsd_vic,
    tidy = .tidy_bcsd_vic
)
