.pull_lcmap <- function(...) {
    base <- paste0(
        "https://edcintl.cr.usgs.gov",
        "/downloads/sciweb1/shared/lcmap",
        "/public/full_extent_downloads"
    )

    df <- dplyr::tribble(
        ~domain, ~domain2, ~version,     ~version2,
        "CU",    "conus",  "version_13", "V13",
        "HI",    "hawaii", "version_10", "V10"
    )

    year <- 1985:2021

    meta <- dplyr::tribble(
        ~varname,    ~description,
        "LCPRI",   "primary-landcover",
        "LCSEC",   "secondary-landcover",
        "LCPCONF", "primary-confidence",
        "LCSCONF", "secondary-confidence",
        "LCACHG",  "cover-change",
        "SCTIME",  "change-day",
        "SCMAG",   "change-magnitude",
        "SCLAST",  "spectral-lastchange",
        "SCSTAB",  "spectral-stability",
        "SCMQA",   "model-quality"
    )

    merge(df, year) |>
        dplyr::rename(year = y) |>
        dplyr::arrange(domain) |>
        merge(meta) |>
        dplyr::mutate(
            URL = glue::glue(
                "/vsicurl/{base}/{version}/{description}_{domain2}_year_data/",
                "LCMAP_{domain}_{year}_{version2}_{varname}/",
                "LCMAP_{domain}_{year}_{version2}_{varname}.tif"
            ),
            asset = glue::glue("{varname}_{domain}_{year}_{version2}"),
            id          = "LCMAP",
            type        = "tif",
            variable    = description,
            description = paste(year, gsub("-", " ", description), domain2, version2),
            units       = "",
            interval    = "1 year",
            nT          = 1,
            tiled       = "T",
            duration    = paste0(year, "-01-01/", year, "-12-31")
        ) |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

.tidy_lcmap <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
        climateR.catalogs::vrt_meta(all = FALSE) |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_lcmap <- climateR.catalogs::data_source$new(
    id   = "lcmap",
    pull = .pull_lcmap,
    tidy = .tidy_lcmap
)
