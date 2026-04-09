.pull_wus_hsp <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_children(
        parent_url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/WUS_HSP",
        id = "WUS_HSP"
    ))
}

.tidy_wus_hsp <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(
            # Variable is the part after the asset prefix + "_"
            # e.g. "SD_A1B_2040s_comp_BASEFLOW" with asset "SD_A1B_2040s" -> "comp_BASEFLOW"
            # e.g. "historical_ET" with asset "historical" -> "ET"
            variable = stringr::str_remove(varname, paste0("^", asset, "_")),
            scenario = asset,
            tiled    = "",
            type     = "zarr"
        ) |>
        arrow::as_arrow_table()
}

ds_wus_hsp <- climateR.catalogs::data_source$new(
    id   = "WUS_HSP",
    pull = .pull_wus_hsp,
    tidy = .tidy_wus_hsp
)
