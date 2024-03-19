.pull_cabcm <- function(...) {
  .tbl = arrow::as_arrow_table(dplyr::bind_rows(
    climateR::read_dap_file(
      "https://cida.usgs.gov/thredds/dodsC/CA-BCM-2014/historical",
      id = "cabcm"
    ),
    climateR::read_dap_file(
      "https://cida.usgs.gov/thredds/dodsC/CA-BCM-2014/future",
      id = "cabcm"
    )
  ))

  return(.tbl)
}

.tidy_cabcm <- function(.tbl, ...) {
  dplyr::as_tibble(.tbl) |>
    dplyr::mutate(varname = gsub("HST_", "HST_HST_", varname),
                  varname = gsub("GISS_AOM", "GISS-AOM", varname)) |>
    tidyr::separate_wider_delim(
      cols        = "varname",
      names       = c("model", "scenario", "BAD", "variable"),
      delim       = "_",
      too_many = "merge",
      cols_remove = FALSE
    ) |>
    dplyr::mutate(tiled = "T", type = "opendap", BAD = NULL,
                  varname = gsub("GISS-AOM", "GISS_AOM", varname),
                  varname = gsub("HST_HST", "HST", varname),
                  model = gsub("GISS-AOM", "GISS_AOM", model)) |>
    arrow::as_arrow_table()

}

ds_cabcm <- climateR.catalogs::data_source$new(
  id   = "cabcm",
  pull = .pull_cabcm,
  tidy = .tidy_cabcm
)
