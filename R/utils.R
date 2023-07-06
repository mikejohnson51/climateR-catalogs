utils::globalVariables(".data")

#' @export
schema = c("id", "asset", "URL", "type", "varname", "variable", "description", "units",
           "model", "ensemble", "scenario",
           "T_name","duration", "interval", "nT",
           "X_name", "Y_name", "X1", "Xn", "Y1", "Yn", "resX", "resY", "ncols", "nrows",
           "crs", "toptobottom", "tiled", "dim_order")

#' @export
arrow_schema <- arrow::schema(
  id          = arrow::dictionary(arrow::int32(), arrow::string()),
  asset       = arrow::string(),
  URL         = arrow::string(),
  type        = arrow::dictionary(arrow::int8(), arrow::string()),
  varname     = arrow::string(),
  variable    = arrow::dictionary(arrow::int32(), arrow::string()),
  description = arrow::string(),
  units       = arrow::string(),
  model       = arrow::string(),
  ensemble    = arrow::string(),
  scenario    = arrow::string(),
  T_name      = arrow::string(),
  duration    = arrow::string(),
  interval    = arrow::string(),
  nT          = arrow::uint32(),
  X_name      = arrow::string(),
  Y_name      = arrow::string(),
  X1          = arrow::float64(),
  Xn          = arrow::float64(),
  Y1          = arrow::float64(),
  Yn          = arrow::float64(),
  resX        = arrow::float64(),
  resY        = arrow::float64(),
  ncols       = arrow::uint64(),
  nrows       = arrow::uint64(),
  crs         = arrow::string(),
  toptobottom = arrow::bool(),
  tiled       = arrow::dictionary(arrow::int8(), arrow::string()),
  dim_order   = arrow::dictionary(arrow::int8(), arrow::string())
)

#' @export
fix_schema <- function(.tbl) {
  .tbl <- dplyr::as_tibble(.tbl)

  schema_names <- names(arrow_schema)
  data_names   <- names(.tbl)
  diff_names   <- setdiff(schema_names, data_names)

  # Ensure schema matches specified arrow schema
  if (length(diff_names) > 0) {
    .tbl[, diff_names] <- NA
  }

  # Ensure `asset` is not all NA
  if (all(is.na(.tbl$asset))) {
    .tbl$asset <- tools::file_path_sans_ext(basename(.tbl$URL))
  }

  # Get missing columns, aka cols with values NA
  missing_cols <- names(.tbl)[which(colSums(is.na(.tbl)) == nrow(.tbl))]
  if (length(missing_cols) > 0) {
      warning("Some schema missing: ",
              paste(missing_cols, collapse = ", "),
              call. = FALSE)
  }

  # Fix factor columns
  .tbl <- dplyr::select(.tbl, dplyr::all_of(schema_names)) |>
          dplyr::mutate(dplyr::across(
              .cols = c(
                   id,
                   type,
                   variable,
                   tiled,
                   dim_order
               ),
              .fns = as.factor
          )) |>
          arrow::as_arrow_table()

  .tbl$cast(arrow_schema)
}

#' @export
read_tds <- function(URL, id, append = ".nc") {
  dat <- rvest::read_html(URL)
  dat <- rvest::html_nodes(dat, "a")

  dat <- data.frame(link = rvest::html_attr(dat, "href"))
  dat$id = id

  dat$link <- gsub(".*=", "", dat$link)

  dat$URL <- paste0(dirname(URL), "/dodsC/", dat$link, append)

  dat[!grepl("http|https|html", dat$link), ]
}

export_catalog = function(x, path){

  x2 = dplyr::bind_rows(x)

  jsonlite::write_json(x2,
                       path = paste0(path, ".json"),
                       pretty = TRUE)

  arrow::write_parquet(x2, paste0(path, ".parquet"))

  saveRDS(x2, file = paste0(path, ".rds"))

  paste0(path, ".rds")
}

vrt_meta = function(data, all = TRUE){

  if(all){
    for(i in 1:nrow(data)){
      r   = terra::rast(data$URL[i])
      data$X1[i]     = terra::xmin(r)
      data$Xn[i]     = terra::xmax(r)
      data$Y1[i]     = terra::xmin(r)
      data$Yn[i]     = terra::ymax(r)
      data$resX[i]   = terra::xres(r)
      data$resY[i]   = terra::yres(r)
      data$ncols[i]  = terra::ncol(r)
      data$nrows[i]  = terra::nrow(r)
      data$crs[i]    = sf::st_crs(r)$proj4string
    }
  } else {
    r           = terra::rast(data$URL[1])
    data$X1     = terra::xmin(r)
    data$Xn     = terra::xmax(r)
    data$Y1     = terra::xmin(r)
    data$Yn     = terra::ymax(r)
    data$resX   = terra::xres(r)
    data$resY   = terra::yres(r)
    data$ncols  = terra::ncol(r)
    data$nrows  = terra::nrow(r)
    data$crs   = sf::st_crs(r)$proj4string
  }


  data
}

polaris_urls = function(base = 'http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/'){

  ids = base |>
    read_html() |>
    html_nodes("a") |>
    html_attr("href")

  ids = grep('vrt$',ids, value = TRUE)

  polaris_describe(data.frame(URL = glue("/vsicurl/{base}{ids}")))

}

polaris_describe = function(data){

  parse_polaris_description = function(x){

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

    v
  }


  ids = gsub(".vrt", "", basename(data$URL))

  out = lapply(ids, parse_polaris_description) %>%
    dplyr::bind_rows()

  cbind(data, out)

}


