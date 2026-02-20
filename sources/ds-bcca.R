.pull_bcca <- function(...) {
    arrow::as_arrow_table(climateR.catalogs::read_stac_children(
        parent_url = "https://api.water.usgs.gov:443/gdp/pygeoapi/stac/stac-collection/bcca",
        id = "bcca"
    ))
}

.tidy_bcca <- function(.tbl, ...) {
    tbl <- dplyr::as_tibble(.tbl)

    # CMIP5 children: varnames like "BCCA_0-125deg_pr_day_MODEL_SCENARIO_ENSEMBLE"
    cmip5 <- tbl |>
        dplyr::filter(grepl("^BCCA_", varname)) |>
        tidyr::separate_wider_delim(
            cols  = "varname",
            names = c(NA, NA, "variable", NA, "model", "scenario", "ensemble"),
            delim = "_",
            too_many = "merge",
            cols_remove = FALSE
        )

    # CMIP3 children: "model-calendar-scenario-run-variable-BCCA_0-125deg"
    # Model names contain underscores, so use regex extraction
    cmip3_match <- stringr::str_match(
        tbl$varname[grepl("^(?!BCCA_)", tbl$varname, perl = TRUE)],
        "^(.+?)-(gregorian|noleap|365_day|proleptic_gregorian|standard)-(.+?)-(.+?)-(.+?)-BCCA.*$"
    )
    cmip3 <- tbl |>
        dplyr::filter(!grepl("^BCCA_", varname)) |>
        dplyr::mutate(
            model    = cmip3_match[, 2],
            scenario = cmip3_match[, 4],
            ensemble = cmip3_match[, 5],
            variable = cmip3_match[, 6]
        )

    dplyr::bind_rows(cmip5, cmip3) |>
        dplyr::mutate(tiled = "T", type = "zarr") |>
        arrow::as_arrow_table()
}

ds_bcca <- climateR.catalogs::data_source$new(
    id   = "bcca",
    pull = .pull_bcca,
    tidy = .tidy_bcca
)
