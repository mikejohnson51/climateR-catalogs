
#' @keywords internal
.pull_bcsd_nmme <- function(...) {
 x = 'http://thredds.northwestknowledge.net:8080/thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/dailyForecasts/catalog.html' |>
    climateR.catalogs::read_tds(id = "bcsd_nmme", append = "") |>
    dplyr::mutate( link = basename(link),
                   URL = paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/dailyForecasts/', link),
                   link = gsub('bcsd_nmme_metdata_', "", link),
                   link = gsub('_forecast', "", link),
                   link = gsub('_daily.nc', "", link))

  stringr::str_match(x$link, '(.*)_(.*)')[, -1] |>
    as.data.frame() |>
    setNames(c("model", "varname")) |>
    dplyr::bind_cols(x) |>
    tidyr::drop_na() |>
    arrow::as_arrow_table()
}

#' @keywords internal
.tidy_bcsd_nmme <- function(.tbl, ...) {

  .tbl <- dplyr::collect(.tbl)

  out <- list()

  for (i in seq_len(nrow(.tbl))) {
    out[[i]] <- tryCatch({
      climateR::read_dap_file(
        URL     = .tbl$URL[i],
        id      = .tbl$varname[i],
        varmeta = TRUE
      )
    }, error = function(condition) NULL)
  }

  .tbl$URL <- NULL

  dplyr::bind_rows(out) |>
    dplyr::rename(variable = id) |>
    dplyr::bind_cols(dplyr::select(.tbl, "model")) |>
    dplyr::mutate(tiled = "", type = "opendap") |>
    arrow::as_arrow_table()
}

#' bcsd_nmme Data Source
ds_bcsd_nmme <- climateR.catalogs::data_source$new(
  id   = "bcsd_nmme",
  pull = .pull_bcsd_nmme,
  tidy = .tidy_bcsd_nmme
)
