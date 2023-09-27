#' @keywords internal
.pull_cfsv2_gridmet <- function(...) {

  paste0("http://thredds.northwestknowledge.net:8080/",
             "thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/cfsv2_metdata_90day/",
             "catalog.html") |>
    climateR.catalogs::read_tds(id = "cfsv2_gridmet", append = "") |>
    filter(grepl("daily.nc", URL)) |>
    mutate(URL = paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/cfsv2_metdata_90day/',
                        basename(URL)), link = NULL) |>
    arrow::as_arrow_table()
}

#' @keywords internal
.tidy_cfsv2_gridmet <- function(.tbl, ...) {
  .tbl <- dplyr::collect(.tbl)

  out <- list()
  for (i in seq_len(nrow(.tbl))) {
    out[[i]] <- tryCatch({
      climateR::read_dap_file(
        URL     = .tbl$URL[i],
        id      = .tbl$id[i],
        varmeta = TRUE
      )
    }, error = function(condition) NULL)
  }

  dplyr::bind_rows(out) |>
  dplyr::mutate(tiled = "",
                type = "opendap",
                ensemble = "median CFS forecast over 48 ensemble members",
                description = paste("28 day (from today) outlook of mean",
                                    gsub("_", " ", varname)),
                duration = "../..") |>
  arrow::as_arrow_table()
}

#' GRIDMET Data Source
ds_cfsv2_gridmet <- climateR.catalogs::data_source$new(
  id   = "cfsv2_gridmet",
  pull = .pull_cfsv2_gridmet,
  tidy = .tidy_cfsv2_gridmet
)

