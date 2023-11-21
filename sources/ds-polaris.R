.parse_polaris_description <- function(x) {
    v = c('silt', 'silt percentage', '%',
          'sand', 'sand percentage', '%',
          'clay', 'clay percentage', '%',
          'bd', 'bulk density', 'g/cm3',
          'theta_s', 'saturated soil water content', 'm3/m3',
          'theta_r', 'residual soil water content', 'm3/m3',
          'ksat', 'saturated hydraulic conductivity', 'log10(cm/hr)',
          'ph', 'soil pH in H2O', 'N/A',
          'om', 'organic matter', 'log10(%)',
          'lambda', 'pore size distribution index (brooks-corey)', 'N/A',
          'hb', 'bubbling pressure (brooks-corey)', 'log10(kPa)',
          'n', 'measure of the pore size distribution (van genuchten)', 'N/A',
          'alpha', 'scale parameter inversely proportional to mean pore diameter (van genuchten)', 'log10(kPa-1)')

    v = matrix(v, ncol = 3, byrow = T) |>
      as.data.frame() |>
      setNames(c('variable', "description", "units"))

    v$id = "polaris"

    m = strsplit(x, "_")[[1]]

    if(length(m) == 5){
      m = c(paste0(m[1], "_", m[2]), m[3], m[4], m[5])
    }

    v = v[v$variable == m[1], ]

    v$variable = paste0(m[2]," ", m[1], " ", m[3], "-", m[4],  'cm' )
    v$asset = paste0(m[2]," ", m[1])
    v
  }

.pull_polaris <- function(...) {

    base <- "http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/"

    ids  <- rvest::read_html(base) |>
            rvest::html_nodes("a") |>
            rvest::html_attr("href") |>
            grep(pattern = "vrt$", value = TRUE)

    .tbl <- data.frame(URL = paste0("/vsicurl/", base, ids))

    ids  <- gsub(".vrt", "", basename(.tbl$URL))

    out <- lapply(ids, .parse_polaris_description) |>
           dplyr::bind_rows()

    .tbl = arrow::as_arrow_table(cbind(.tbl, out))

    return(.tbl)
}

.tidy_polaris <- function(.tbl, ...) {
   plyr::as_tibble(.tbl) |>
      dplyr::mutate(type = "vrt", tiled = "", varname = variable) |>
      climateR.catalogs::vrt_meta(all = FALSE) |>
      arrow::as_arrow_table()
}

ds_polaris <- climateR.catalogs::data_source$new(
    id   = "polaris",
    pull = .pull_polaris,
    tidy = .tidy_polaris
)
