#' @keywords internal
.pull_cfsv2_gridmet_median <- function(...) {
  .tbl = paste0(
    "http://thredds.northwestknowledge.net:8080/",
    "thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/cfsv2_metdata_90day/",
    "catalog.html"
  ) |>
    climateR.catalogs::read_tds(id = "cfsv2_gridmet", append = "") |>
    dplyr::filter(grepl("daily.nc", URL)) |>
    dplyr::mutate(
      URL = paste0(
        'http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/cfsv2_metdata_90day/',
        basename(URL)
      ),
      link = NULL
    )

  arrow::as_arrow_table(.tbl)
}

#' @keywords internal
.tidy_cfsv2_gridmet_median <- function(.tbl, ...) {
  .tbl <- dplyr::collect(.tbl)

  out <- list()

  for (i in seq_len(nrow(.tbl))) {
    out[[i]] <- tryCatch({
      climateR::read_dap_file(URL     = .tbl$URL[i],
                              id      = .tbl$id[i],
                              varmeta = TRUE)
    }, error = function(condition)
      NULL)
  }

  .tbl = dplyr::bind_rows(out) |>
    dplyr::mutate(
      tiled = "",
      type = "opendap",
      ensemble = "median",
      description = paste("28 day (from today) outlook of mean",
                          gsub("_", " ", varname)),
      duration = "../..",
      variable = gsub(
        "cfsv2_metdata_forecast_",
        "",
        gsub("_daily.nc" , "", basename(URL))
      ),
      asset = glue::glue("ensemble_median")
    )

  arrow::as_arrow_table(.tbl)
}

#' cfsv2_gridmet Data Source
ds_cfsv2_gridmet_median <- climateR.catalogs::data_source$new(id   = "cfsv2_gridmet",
                                                              pull = .pull_cfsv2_gridmet_median,
                                                              tidy = .tidy_cfsv2_gridmet_median)

### ---------------------------

#' @keywords internal
.pull_cfsv2_gridmet_member <- function(...) {
  .tbl = paste0(
    "http://thredds.northwestknowledge.net:8080/",
    "thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/cfsv2_metdata_90day/",
    "catalog.html"
  ) |>
    climateR.catalogs::read_tds(id = "cfsv2_gridmet", append = "") |>
    dplyr::filter(grepl("_1.nc$|_2.nc|_0.nc", URL)) |>
    dplyr::mutate(
      URL = paste0(
        'http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/cfsv2_metdata_90day/',
        basename(URL)
      ),
      link = basename(link),
    ) |>
    tidyr::separate_wider_delim(
      cols  = "link",
      names = c(NA, NA, NA, "variable", NA, "hour", "ensemble", "day"),
      delim = "_"
    ) |>
    dplyr::mutate(day = gsub(".nc", "", day))

  arrow::as_arrow_table(.tbl)
}

#' @keywords internal
.tidy_cfsv2_gridmet_member <- function(.tbl, ...) {
  .tbl <- dplyr::collect(.tbl)

  tmp = .tbl |>
    dplyr::group_by(variable) |>
    dplyr::slice(1)

  out <- list()

  for (i in seq_len(nrow(tmp))) {
    out[[i]] <- tryCatch({
      climateR::read_dap_file(URL     = tmp$URL[i],
                              id      = tmp$variable[i],
                              varmeta = TRUE)
    }, error = function(condition)
      NULL)
  }

  .tbl = dplyr::bind_rows(out) |>
    dplyr::rename(variable = id) |>
    dplyr::mutate(URL = NULL) |>
    dplyr::right_join(.tbl, by = "variable") |>
    dplyr::mutate(
      tiled = "",
      type = "opendap",
      description = glue::glue(
        "{day} day old (from today) 28 day forecast of {variable} made at {hour}H for ensemble member {ensemble} of 48"
      ),
      duration = "../..",
      asset = glue::glue("{day}_{hour}_{ensemble}"),
      day = NULL
    )

  arrow::as_arrow_table(.tbl)
}

#' cfsv2_gridmet Data Source
ds_cfsv2_gridmet_member <- climateR.catalogs::data_source$new(id   = "cfsv2_gridmet",
                                                              pull = .pull_cfsv2_gridmet_member,
                                                              tidy = .tidy_cfsv2_gridmet_member)
