utils::globalVariables(".data")

#' @export
schema = c("id", "asset", "URL", "type", "varname", "variable", "description", "units",
           "model", "ensemble", "scenario",
           "T_name","duration", "interval", "nT",
           "X_name", "Y_name", "X1", "Xn", "Y1", "Yn", "resX", "resY", "ncols", "nrows",
           "crs", "toptobottom", "tiled", "dim_order")

#' @export
arrow_schema <- function() {
  arrow::schema(
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
}

#' @export
rectify_schema <- function(.tbl) {
  .tbl <- dplyr::as_tibble(.tbl)

  schema_names <- names(arrow_schema())
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

  .tbl$cast(arrow_schema())
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

#' @export
vrt_meta <- function(data, all = TRUE) {

  if (all) {
    for (i in seq_len(nrow(data))) {
      r             = terra::rast(data$URL[i])
      data$X1[i]    = terra::xmin(r)
      data$Xn[i]    = terra::xmax(r)
      data$Y1[i]    = terra::xmin(r)
      data$Yn[i]    = terra::ymax(r)
      data$resX[i]  = terra::xres(r)
      data$resY[i]  = terra::yres(r)
      data$ncols[i] = terra::ncol(r)
      data$nrows[i] = terra::nrow(r)
      data$crs[i]   = sf::st_crs(r)$proj4string
    }
  } else {
    r          = terra::rast(data$URL[1])
    data$X1    = terra::xmin(r)
    data$Xn    = terra::xmax(r)
    data$Y1    = terra::xmin(r)
    data$Yn    = terra::ymax(r)
    data$resX  = terra::xres(r)
    data$resY  = terra::yres(r)
    data$ncols = terra::ncol(r)
    data$nrows = terra::nrow(r)
    data$crs   = sf::st_crs(r)$proj4string
  }

  data
}

#' Parse ISO 8601 duration string to human-readable interval
#' @param step ISO 8601 duration string (e.g. "P1DT0H0M0S", "P0Y1M0DT0H0M0S")
#' @return Character string like "1 day", "1 month", "1 year"
#' @export
parse_iso8601_duration <- function(step) {
  if (is.null(step) || is.na(step) || step == "NaT") return(NA_character_)
  # Numeric step (nanoseconds, e.g. ssebopeta_daily has 8.64e+13 = 1 day in ns)
  if (is.numeric(step)) {
    days <- step / 8.64e13
    if (abs(days - round(days)) < 0.01) {
      return(paste(round(days), "days"))
    }
    return(NA_character_)
  }
  m <- regmatches(step, regexec("^P(?:(\\d+)Y)?(?:(\\d+)M)?(?:(\\d+)D)?(?:T.*)?$", step, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_character_)
  years  <- as.integer(ifelse(m[2] == "", "0", m[2]))
  months <- as.integer(ifelse(m[3] == "", "0", m[3]))
  days   <- as.integer(ifelse(m[4] == "", "0", m[4]))
  if (years > 0)  return(paste(years, "years"))
  if (months > 0) return(paste(months, "months"))
  if (days > 0)   return(paste(days, "days"))
  NA_character_
}

#' Read a STAC collection and build catalog rows
#' @param url Full URL to the STAC collection endpoint
#' @param id Catalog identifier for this data source
#' @param asset_name Name of the asset key for the zarr store (default "zarr-s3-osn")
#' @return data.frame with one row per variable, columns matching catalog schema
#' @export
read_stac_collection <- function(url, id, asset_name = "zarr-s3-osn") {
  json <- jsonlite::fromJSON(url, simplifyVector = FALSE)

  cube_vars <- json[["cube:variables"]]
  cube_dims <- json[["cube:dimensions"]]
  if (is.null(cube_vars) || is.null(cube_dims)) {
    warning("No cube:variables or cube:dimensions in ", url)
    return(data.frame())
  }

  # Filter to data variables with 3+ spatial/temporal dims
  is_data_var <- vapply(cube_vars, function(v) {
    v$type == "data" && length(v$dimensions) >= 3
  }, logical(1))
  cube_vars <- cube_vars[is_data_var]
  if (length(cube_vars) == 0) return(data.frame())

  # Extract spatial/temporal dimensions
  x_dim <- y_dim <- t_dim <- NULL
  for (nm in names(cube_dims)) {
    d <- cube_dims[[nm]]
    if (identical(d$type, "temporal")) t_dim <- list(name = nm, d = d)
    if (identical(d$axis, "x"))        x_dim <- list(name = nm, d = d)
    if (identical(d$axis, "y"))        y_dim <- list(name = nm, d = d)
  }
  if (is.null(x_dim) || is.null(y_dim)) {
    warning("Missing x/y dimensions in ", url)
    return(data.frame())
  }

  # Spatial metadata
  X1   <- x_dim$d$extent[[1]]
  Xn   <- x_dim$d$extent[[2]]
  Y1   <- y_dim$d$extent[[1]]
  Yn   <- y_dim$d$extent[[2]]
  resX <- abs(x_dim$d$step)
  resY <- abs(y_dim$d$step)
  ncols <- round((Xn - X1) / resX) + 1
  nrows <- round((Yn - Y1) / resY) + 1
  toptobottom <- if (!is.null(y_dim$d$step) && y_dim$d$step < 0) TRUE else FALSE

  # CRS from reference_system
  crs_str <- NA_character_
  ref_sys <- x_dim$d$reference_system
  if (!is.null(ref_sys)) {
    if (is.character(ref_sys)) {
      ref_json <- tryCatch(jsonlite::fromJSON(ref_sys, simplifyVector = FALSE), error = function(e) NULL)
    } else {
      ref_json <- ref_sys
    }
    if (!is.null(ref_json) && !is.null(ref_json$id)) {
      crs_str <- paste0(ref_json$id$authority, ":", ref_json$id$code)
    }
  }

  # Temporal metadata
  duration <- NA_character_
  interval <- NA_character_
  nT       <- NA_integer_
  T_name   <- NA_character_
  if (!is.null(t_dim)) {
    T_name <- t_dim$name
    t_start <- t_dim$d$extent[[1]]
    t_end   <- t_dim$d$extent[[2]]
    duration <- paste(t_start, t_end, sep = "/")
    interval <- parse_iso8601_duration(t_dim$d$step)
    # Compute nT from duration and interval
    if (!is.na(interval)) {
      t0 <- as.POSIXct(t_start, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
      t1 <- as.POSIXct(t_end, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
      if (!is.na(t0) && !is.na(t1)) {
        if (grepl("month", interval)) {
          nT <- as.integer(round(lubridate::interval(t0, t1) / months(1))) + 1L
        } else if (grepl("day", interval)) {
          nT <- as.integer(difftime(t1, t0, units = "days")) + 1L
        } else if (grepl("year", interval)) {
          nT <- as.integer(round(lubridate::interval(t0, t1) / lubridate::years(1))) + 1L
        }
      }
    }
  }

  # Zarr URL: convert s3:// to https://
  zarr_url <- NA_character_
  asset <- json[["assets"]][[asset_name]]
  if (!is.null(asset)) {
    s3_href <- asset$href
    zarr_url <- sub("^s3://", "https://usgs.osn.mghpcc.org/", s3_href)
  }

  # Build data.frame with one row per variable
  rows <- lapply(names(cube_vars), function(vname) {
    v <- cube_vars[[vname]]
    data.frame(
      id          = id,
      asset       = json[["id"]],
      URL         = zarr_url,
      type        = "zarr",
      varname     = vname,
      description = if (!is.null(v$description)) v$description else NA_character_,
      units       = if (!is.null(v$unit)) v$unit else NA_character_,
      T_name      = T_name,
      duration    = duration,
      interval    = interval,
      nT          = nT,
      X_name      = x_dim$name,
      Y_name      = y_dim$name,
      X1          = X1,
      Xn          = Xn,
      Y1          = Y1,
      Yn          = Yn,
      resX        = resX,
      resY        = resY,
      ncols       = ncols,
      nrows       = nrows,
      crs         = crs_str,
      toptobottom = toptobottom,
      tiled       = "",
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

#' Read all child collections from a parent STAC collection
#' @param parent_url Full URL to the parent STAC collection
#' @param id Catalog identifier for this data source
#' @return data.frame with rows from all child collections combined
#' @export
read_stac_children <- function(parent_url, id) {
  json <- jsonlite::fromJSON(parent_url, simplifyVector = FALSE)
  child_links <- Filter(function(l) identical(l$rel, "child"), json$links)

  if (length(child_links) == 0) {
    warning("No child links found in ", parent_url)
    return(data.frame())
  }

  results <- lapply(child_links, function(link) {
    tryCatch(
      read_stac_collection(link$href, id = id),
      error = function(e) {
        warning("Failed to read child ", link$href, ": ", e$message)
        data.frame()
      }
    )
  })

  dplyr::bind_rows(results)
}
