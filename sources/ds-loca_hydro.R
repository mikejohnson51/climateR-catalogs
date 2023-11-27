.pull_loca_hydro <- function(...) {
    url <- "ftp://gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/loca_hydro/LOCA_VIC_dpierce_2017-02-28/"
    result <- RCurl::getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)

    models = strsplit(result, "\\n")[[1]]

    url2 = paste0(url, models[1], "/")
    result2 <- RCurl::getURL(url2,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
    scenarios = strsplit(result2, "\\n")[[1]]

    url3 = paste0(url2, "/", scenarios[1], "/")
    result3 <- RCurl::getURL(url3,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
    tmp = strsplit(strsplit(result3, "\\n")[[1]], "\\.")
    meta = data.frame(do.call(rbind, tmp)) |>
        dplyr::filter(X2 != "nc")

    g1 = expand.grid(models, scenarios[1], unique(meta$X1), unique(readr::parse_number(meta$X2)), KEEP.OUT.ATTRS = TRUE, stringsAsFactors = FALSE)

    url3 = paste0(url2, "/", scenarios[2], "/")
    result4 <- RCurl::getURL(url3,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
    tmp = strsplit(strsplit(result4, "\\n")[[1]], "\\.")
    meta = data.frame(do.call(rbind, tmp)) |>
        dplyr::filter(X2 != "nc")

    g2 = expand.grid(models, scenarios[2:3], unique(meta$X1), unique(readr::parse_number(meta$X2)), KEEP.OUT.ATTRS = TRUE, stringsAsFactors = FALSE)

    g3 = data.frame(rbind(g1, g2)) |>
        dplyr::distinct() |>
        tidyr::drop_na()

    names(g3) = c("model", "scenario", "varname", "year")

    .tbl = dplyr::group_by(g3, model, scenario, varname) |>
        dplyr::mutate(interval = "1 day", duration = paste0(min(year), "-01-01/", max(year), "-12-31"),
            id = "loca_hydrology",
            URL = paste0("/vsicurl/", url, model,"/", scenario, "/", varname, ".{year}.v0.nc"),
            scenario = gsub(".netcdf", "", gsub("vic_output.", "", scenario)),
            type = "ftp", variable = varname, description = varname, toptobottom = FALSE, tiled = "T", T_name = "Time",
            X_name = "Lon", Y_name = "Lat", nT = 365) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        arrow::as_arrow_table()

    return(.tbl)
}

# ---------------------------------------------------------------------
#

terra::rast('/vsicurl/ftp://gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/loca_hydro/LOCA_VIC_dpierce_2017-02-28/ACCESS1-0/vic_output.historical.netcdf/ET.1950.v0.nc')

.tidy_loca_hydro <- function(.tbl, ...) {
    x <- dplyr::as_tibble(.tbl)

    x2 <- dplyr::group_by(x, varname) |>
          dplyr::slice(1) |>
          dplyr::mutate(URL = glue::glue(URL)) |>
          dplyr::ungroup() |>
          dplyr::select(URL, varname)

    for(i in seq_len(nrow(x2))){
        r           = terra::rast(x2$URL[i], lyrs = 1)
        x2$units[i] = terra::units(r)
        x2$crs[i]   = sf::st_crs(r)$proj4string
        x2$X1[i]    = terra::xmin(r)
        x2$Xn[i]    = terra::xmax(r)
        x2$Y1[i]    = terra::ymin(r)
        x2$Yn[i]    = terra::ymax(r)
        x2$resX[i]  = terra::res(r)[1]
        x2$resY[i]  = terra::res(r)[2]
        x2$ncols[i] = ncol(r)
        x2$nrows[i] = nrow(r)
    }

    dplyr::left_join(x, dplyr::select(x2, -URL), by = "varname") |>
        arrow::as_arrow_table()
}

# ---------------------------------------------------------------------

ds_loca_hydro <- climateR.catalogs::data_source$new(
    id   = "loca_hydro",
    pull = .pull_loca_hydro,
    tidy = .tidy_loca_hydro
)
