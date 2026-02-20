.pull_topowx <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_children(
        parent_url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/TopoWx2017",
        id = "topowx"
    ))
}

.tidy_topowx <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        dplyr::mutate(
            variable = varname,
            tiled    = "",
            type     = "zarr",
            id       = dplyr::case_when(
                asset == "topowx"         ~ "topowx_daily",
                asset == "topowx_monthly" ~ "topowx_monthly",
                asset == "topowx_normals" ~ "topowx_normals",
                .default = id
            )
        ) |>
        arrow::as_arrow_table()
}

ds_topowx <- climateR.catalogs::data_source$new(
    id   = "topowx",
    pull = .pull_topowx,
    tidy = .tidy_topowx
)
