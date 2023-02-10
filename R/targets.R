# Data Source 1 -----------------------------------------------------------
get_maca = function(id = "maca"){
 bind_rows(
    read_tds(URL = "http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html",
             id = glue("{id}_day")),
    read_tds("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html",
              id = glue("{id}_month"))
  ) %>%
    separate_wider_delim(link,
                    names = c(NA, NA, "variable", "model", "ensemble", "scenario", NA, NA, NA, NA),
                    delim = "_",
    ) %>%
    opendap.catalog::dap_meta() %>%
    mutate(tiled = "T", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)


}

# Data Source 2 -----------------------------------------------------------
get_gridmet <- function(id = "gridmet"){
  read_tds("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html", id) %>%
    separate_wider_delim(link,
                  names = c(NA, NA, "variable", NA, NA, NA),
                  delim = "_") |>
  opendap.catalog::dap_meta() |>
  mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
  rectify_schema(schema)


}

# Data Source 3 -----------------------------------------------------------
get_terraclim = function(id = "terraclim"){
  read_tds("http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html", id) %>%
    separate_wider_delim(link, names = c(NA, NA, "variable", NA, NA, NA), delim = "_", too_few = "align_end") %>%
    opendap.catalog::dap_meta() %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 4 -----------------------------------------------------------
get_terraclim_normals = function(id = "terraclim_normals"){

 read_tds('http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/summaries/catalog.html', id,
          "") |>
  mutate(URL = paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/summaries/', basename(link))) |>
  mutate(link2 = gsub(".nc", "", basename(link))) %>%
  filter(link2 != "summaries") |>
  separate_wider_delim(link2, names = c("scenario", 'variable'), delim = "_") %>%
  mutate(scenario = gsub("TerraClimate", "", scenario)) %>%
  filter(!is.na(variable)) |>
  opendap.catalog::dap_meta() |>
  mutate(tiled = "", interval = "1 month", type = "opendap", crs = proj, description = long_name) %>%
  rectify_schema(schema)

}

# Data Source 5 -----------------------------------------------------------
get_vic = function(id = "vic"){
  read_tds(URL = "https://www.reacchpna.org/thredds/nw.csc.hydrology-vic.aggregated.html", id) %>%
    separate_wider_delim(link, names = c(NA, "variable", "model", "ensemble", "scenario", NA, NA, NA, NA, NA, NA), delim = "_", too_few = "align_end") %>%
    opendap.catalog::dap_meta() %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 6 -----------------------------------------------------------
get_daymet4 = function(id = "daymet4"){

  urls = read_tds(URL = "https://thredds.daac.ornl.gov/thredds/catalog/daymet-v4-agg/catalog.html", id) %>%
    mutate(URL = paste0('https://thredds.daac.ornl.gov/thredds/dodsC/', link)) %>%
    filter(grepl("ncml", URL))

  out = list()

  for(j in 1:length(urls)){
    out[[j]] = ncmeta::nc_vars(urls$URL[j]) %>%
        filter(ndims == 3) %>%
        select(variable = name) %>%
        mutate(URL = urls$URL[j], id = id, model = gsub(".ncml", "",basename(urls$URL[j]))) %>%
        opendap.catalog::dap_meta() %>%
        mutate(tiled = "", variable = varname, type = "opendap", crs = proj, description = long_name)
  }

  bind_rows(out) %>%
    distinct() %>%
    rectify_schema(schema)
}

# Data Source 7 -----------------------------------------------------------
get_ldas = function(){
  das = html_nodes(read_html('https://hydro1.gesdisc.eosdis.nasa.gov/dods/'), "a")
  URL = unique(gsub("\\.[a-z]*$","", html_attr(das, "href")))
  URL = URL[grepl("NLDAS|GLDAS", URL)]

  ldas = list()

  for(i in 1:length(URL)){
    ldas[[i]] = opendap.catalog::read_dap_file(paste0(URL[i], "/"), id = basename(URL[i]))
    message(i, " of ", length(URL))
  }

  bind_rows(ldas) |>
    mutate(variable  = varname, tiled = "", crs = proj, description = long_name, type = "opendap") %>%
    separate_wider_delim(id,
                         names = c('id', 'model'),
                         delim = "_",
                         too_many = "merge") %>%
    rectify_schema(schema)
}

# Data Source 8 -----------------------------------------------------------
get_worldclim_hist = function(){

  df <- data.frame(
    source = "WorldClim2.1",
    tiled = "T",
    variable = c("tmin", "tmax", 'tavg', "prec", "srad", 'wind', 'vapr'),
    description = c(
      "minimum temperature",
      "maximum temperature",
      "average temperature",
      "precipitation ",
      "solar radiation ",
      "wind speed",
      "water vapor pressure"
    ),
    units = c(
      "C",
      "C",
      "C",
      "mm",
      "(kJ m-2 day-1",
      "m s-1",
      "kPa"
    ),
    duration = "1970-01-01/2000-01-01",
    interval = "monthly",
    nT = 12
  ) %>%
    slice(rep(1:7, each = 12)) %>%
    mutate(month = rep(1:7, each = 12), varname = variable)

  df1 <- mutate(df, id = "wc2.1_10m")
  df2 <- mutate(df, id = "wc2.1_5m")
  df3 <- mutate(df, id = "wc2.1_2.5m")
  df4 <- mutate(df, id = "wc2.1_30s")


  wc <- bind_rows(df1, df2, df3, df4) |>
    mutate(URL = paste0(
      "/vsizip/{/vsicurl/https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/",
      id, '_',
      varname,
      ".zip}/",
      id,
      "_",
      gsub("BIO", "", varname), "_", sprintf("%02d", month),
      ".tif"
    ))

  dep = group_by(wc, id) |>
    slice(1) |>
    select(URL, id) |>
    ungroup() %>%
    mutate(X1 = NA, Xn = NA, Y1 = NA, Yn = NA, resX = NA, resY = NA, ncols = NA, nrows = NA, crs = NA)

  for (i in 1:nrow(dep)) {

    for(t in 1:10){
      try({
        r <- terra::rast(dep$URL[i])
        break #break/exit the for-loop
      }, silent = TRUE)
    }

    dep$X1[i]    <- terra::xmin(r)
    dep$Xn[i]    <- terra::xmax(r)
    dep$Y1[i]    <- terra::xmin(r)
    dep$Yn[i]    <- terra::ymax(r)
    dep$resX[i]  <- terra::xres(r)
    dep$resY[i]  <- terra::yres(r)
    dep$ncols[i] <- terra::ncol(r)
    dep$nrows[i] <- terra::nrow(r)
    dep$crs[i]   <- sf::st_crs(r)$proj4string

    message("Success! ", dep$URL[i])
  }

  left_join(wc, select(dep, -URL), "id") %>%
    mutate(type = "VRT", toptobottom = FALSE) %>%
    rectify_schema(schema)

}

# Data Source 9 -----------------------------------------------------------
get_loca = function(id = "loca"){
  bind_rows(
    opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_historical", id = id),
    opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_future", id = id)
  ) %>%
    separate_wider_delim(varname,
                    names = c("variable", "model", "ensemble", "scenario"),
                    delim = "_",
                    cols_remove = FALSE) %>%
    mutate(tiled = "T") %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 10 -----------------------------------------------------------
get_bcca <- function(id = "bcca"){

  bind_rows(
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future", id = id),
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical", id = id)
  ) %>%
    separate_wider_delim(varname,
                    names = c(NA, NA, "variable", NA, "model", "scenario", "ensemble"),
                    delim = "_",
                    cols_remove  = FALSE) %>%
    mutate(tiled = "T", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 11 -----------------------------------------------------------
get_bcsd_vic <- function(id = 'bcsd_vic'){
  opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC", id = id) %>%
    separate_wider_delim(varname,
                    names = c("model", "scenario", "ensemble", "variable"),
                    delim = "_",
                    too_many = "merge",
                    cols_remove = FALSE) %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)

}

# Data Source 12 -----------------------------------------------------------
get_bcsd = function(id = "bcsd"){
  opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/bcsd_obs", id = id) %>%
    opendap.catalog::dap_meta() %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 13 -----------------------------------------------------------
get_dcp = function(id = "dcp"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/dcp/conus_t", id = id) %>%
    separate_wider_delim(varname,
                    names = c("model", "scenario", "variable", NA, NA),
                    delim = "_",
                    too_few = "align_end",
                    cols_remove = FALSE) %>%
    mutate(variable = varname, tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 14 -----------------------------------------------------------
get_maurer = function(id = "maurer"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml", id = id) %>%
    separate_wider_delim(varname,
                    names = c("scenario", "model", "ensemble", "variable"),
                    delim = "_",
                    too_many = "merge",
                    cols_remove = FALSE) %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 15 -----------------------------------------------------------
get_ssebopeta = function(id = "ssebopeta"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly", id = id) %>%
    opendap.catalog::dap_meta() %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 16 -----------------------------------------------------------
get_topowx = function(id = "ssebopeta"){
  bind_rows(
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/topowx", id = "topowx_daily"),
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/topowx_monthly", id = "topowx_monthly"),
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/topowx_normals", id = "topowx_normals")) %>%
    mutate(tiled = "", variable = varname, type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 17 -----------------------------------------------------------
get_prism_monthly = function(id = "prism_monthly"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/prism_v2", id = id) %>%
    mutate(variable = varname, tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 18 -----------------------------------------------------------
get_hbv = function(base = '/vsicurl/https://github.com/atsyplenkov/HBVr/releases/download/parameter-maps/correct_fold_'){

  urls = glue('{base}{0:9}.tiff')

  v = c('beta', 'Shape coefficient of recharge function', NA,
        'FC', 'Maximum soil moisture storage', 'mm',
        'K0', 'Recession coefficient of upper zone', 'day−1',
        'K1', 'Recession coefficient of upper zone', 'day−1',
        'K2', 'Recession coefficient of lower zone', 'day−1',
        'LP', 'Soil moisture value above which actual evaporation reaches potential evaporation', NA,
        'PERC', 'Maximum percolation to lower zone', 'mm day−1',
        'UZL', 'Threshold parameter for extra outflow from upper zone', 'mm',
        'TT', 'Threshold temperature', 'C',
        'CFMAX', 'Degree‐day factor', 'mm °−11 day−1)',
        'CFR',  'Refreezing coefficient', NA,
        'CWH', 'Water holding capacity', NA) %>%
    matrix(ncol = 3, byrow = TRUE) %>%
    data.frame()

  names(v) = c('variable', 'description', 'units' )

  data = list()

  for(i in 1:length(urls)){

    r = terra::rast(urls[i])

    df = data.frame(variable = names(r))
    df$id = "HBV"
    df$varname = df$variable
    df$asset = strsplit(basename(terra::sources(r)), "[.]")[[1]][1]
    df$URL = terra::sources(r)
    df$type = "multiband_tif"

    data[[i]] = left_join(df, v, by = "variable")

  }

  bind_rows(data) %>%
    vrt_meta() %>%
    mutate(type = "vrt", tiled = "") %>%
    rectify_schema(schema)

}

# Data Source 19 -----------------------------------------------------------
get_elevation_data = function(){
  data.frame(
    id  = c("USGS 3DEP", "USGS 3DEP", " USGS 3DEP", "GEBCO2019", "NASADEM"),
    asset = c("30m CONUS DEM", "10m CONUS DEM", "60m Alaska DEM", "Global DEM", "Global DEM"),
    URL = c(
      '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt',
      '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt',
      '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/2/TIFF/USGS_Seamless_DEM_2.vrt',
      '/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2019_GEOTIFF/GEBCO_2019.tif',
      '/vsicurl/https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt'
    ),
    varname = rep('elevation', 5),
    variable = rep('elevation', 5),
    units = rep("meters", 5)
  ) %>%
    mutate(description = asset, type = "vrt", tiled = "") %>%
    vrt_meta(all = TRUE) %>%
    rectify_schema(schema)
}

# Data Source 20 -----------------------------------------------------------
get_polaris = function(){

  polaris_urls() %>%
    mutate(type = "vrt", varname= variable, tiled = "") %>%
    vrt_meta(all = FALSE) %>%
    rectify_schema(schema)
}

# Data Source 21 -----------------------------------------------------------
get_nlcd = function(base = "https://storage.googleapis.com/feddata-r/nlcd/"){

  year     = c(2019, 2016, 2011, 2008, 2006, 2004, 2001)
  dataset  = c('Land_Cover', 'Impervious', 'Tree_Canopy')
  landmass = c('L48', 'AK', 'HI', 'PR')

  g = expand.grid(year, dataset, landmass)

  file <- paste0(g$Var1, "_", g$Var2, "_", g$Var3, ".tif")

  df = data.frame(
    URL = paste0(base, file),
    asset = paste(g$Var1, gsub("_", " ", g$Var2), g$Var3),
    varname = g$Var2,
    variable = g$Var2,
    description = paste("USGS NLCD", gsub("_", " ", g$Var2), g$Var1, g$Var3)
  ) %>%
    mutate(units = case_when(
      varname == "Land_Cover" ~ "",
      TRUE ~ "%"
    ))

  for(i in 1:nrow(df)){
    df$exists[i] =  df$URL[i] %>%
      httr::HEAD() %>%
      httr::status_code() %>%
      identical(200L)
  }

  filter(df, exists) %>%
    mutate(URL = paste0("/vsicurl/", URL), id = "NLCD", type = "vrt", tiled = "") %>%
    select(-exists) %>%
    vrt_meta() %>%
    rectify_schema(schema)
}

# Data Source 22 -----------------------------------------------------------
get_isric = function(base = 'https://files.isric.org'){

  ids = glue('{base}/soilgrids/latest/data/') |>
    read_html() |>
    html_nodes("a") |>
    html_attr("href")

  ids = grep("data", ids, value = TRUE)
  ids = grep("/$", ids, value = TRUE)
  ids = grep("/$", ids, value = TRUE)

  outs = list()

  for(i in 1:length(ids)){

    tmp = paste0(base, substring(ids[i],1, nchar(ids[i])-1)) |>
      read_html() |>
      html_nodes("a") |>
      html_attr("href")

    tmp = grep(".vrt$", tmp, value = TRUE)

    outs[[i]] = glue('/vsicurl/{base}{tmp}')
  }

  data.frame(
    URL = unlist(outs),
    id = "ISRIC Soil Grids") %>%
    mutate(varname = gsub(".vrt", "", basename(URL))) %>%
    tidyr::separate(varname,
                    c("variable", "depth", "measure"),
                    sep = "_",
                    extra = "merge",
                    remove = FALSE) %>%
    mutate(description = paste(measure, variable, depth), tiled = "NA", type = "vrt", variable = varname) %>%
    vrt_meta(all = FALSE) %>%
    rectify_schema(schema)

}

# Data Source 23 -----------------------------------------------------------
get_erdap = function(url = "https://irishmarineinstitute.github.io/awesome-erddap/erddaps.json"){
  # Find public servers
  servers <- jsonlite::read_json(url, simplifyVector = TRUE) |>
    filter(public) |>
    mutate(url = paste0(url, "griddap/index.html"))

  # Base functions ...
  .internal_get <- function(ds) {
    xxx <- tryCatch(
      {
        suppressMessages({
          read_dap_file(URL = ds$link, id = ds$id) |>
            mutate(variable = varname, tiled = "")
        })
      },
      error = function(e) {
        NULL
      }
    )

    log_info("\t > ", ds$id, " (", ds$n, " of ", ds$mx, ")")
    xxx
  }

  get_griddap_server <- function(server, overwrite = FALSE) {
    outfile <- paste0("data-raw/", server$short_name, ".rds")

    if (any(!file.exists(outfile), overwrite)) {
      log_info("Checking Server: ", server$short_name)
      serv <- tryCatch(
        {
          html_attr(html_nodes(read_html(server$url), "a"), "href")
        },
        error = function(e) {
          NULL
        }
      )


      if (!is.null(serv)) {
        log_info("Scrapping Server: ", server$short_name)
        ds <- data.frame(link = serv) |>
          filter(grepl("html$", link)) |>
          filter(grepl("griddap", link)) |>
          filter(!grepl("test", link)) |>
          mutate(link = gsub(".html", "", link), id = basename(link)) |>
          filter(id != "documentation")

        log_info(nrow(ds), " datasets found...")

        if (nrow(ds) > 0) {
          ds <- mutate(ds, n = 1:n(), mx = max(n))

          collection <- lapply(1:nrow(ds), function(x) {
            .internal_get(ds = ds[x, ])
          })

          saveRDS(bind_rows(collection), outfile)
        }
      } else {
        logger::log_info("No data found on: ", server$short_name)
      }
    }
  }

  out <- lapply(1:nrow(servers), function(x) {
    get_griddap_server(server = servers[x, ], overwrite = FALSE)
  })

  bind_rows(lapply(list.files("data-raw", full = T), readRDS)) %>%
    mutate(type = "erddap", description = long_name, crs = proj) %>%
    rectify_schema(schema)
}

# Data Source 24 -----------------------------------------------------------
get_modis = function(URL = 'https://opendap.cr.usgs.gov/opendap/hyrax/') {
  modis_hyrax =  html_nodes(read_html(URL), "a")

  modis_hyrax = data.frame(link = html_attr(modis_hyrax, "href")) |>
    mutate(id = dirname(link),
           link = paste0(URL,
                         gsub('contents.html', "", link))) |>
    filter(!grepl("http|4913|opendap|PROTOTYPE", id)) |>
    filter(id != ".") |>
    filter(grepl("MOD", id))


  modis_data = list()

  for (i in 1:nrow(modis_hyrax)) {
    tmp = tryCatch({
      data.frame(link = html_attr(html_nodes(read_html(
        modis_hyrax$link[i]
      ), "a"), "href"))
    }, error = function(e) {
      NULL
    })

    if (!is.null(tmp)) {
      modis_data[[i]]  = data.frame(link = html_attr(html_nodes(read_html(
        modis_hyrax$link[i]
      ), "a"), "href")) |>
        filter(grepl(".ncml.dap", link)) |>
        mutate(
          id = modis_hyrax$id[i],
          tile = gsub('.ncml.dap', "", link),
          link = modis_hyrax$link[i]
        )
    } else {
      modis_data[[i]] = NULL
    }

    message(i, " of ", nrow(modis_hyrax))
  }

  modis_collection = bind_rows(modis_data) |>
    filter(id != "") %>%
    group_by(id) |>
    mutate(
      mosaic = n(),
      tiled = ifelse(mosaic > 1, "XY_modis", ""),
      tile  = ifelse(tile == "", id, tile)
    ) |>
    ungroup() |>
    mutate(tmp = paste0(link, tile, ".ncml#fillmismatch"))

  tmp = group_by(modis_collection, id) %>%
    slice(1) %>%
    ungroup()

  modis_param = lapply(1:nrow(tmp), function(x) {
    p = tryCatch({
      nc =  open.nc(tmp$tmp[x])
      raw = dap_xyzv(obj = nc, varmeta = TRUE)
      raw$id = "MODIS"
      raw$asset  = tmp$id[x]
      raw$tiled = tmp$tiled[x]

      merge(raw,
            data.frame(
              opendap.catalog:::.resource_time(nc, raw$T_name[1]),
              asset = tmp$id[x]
            ) ,
            by = 'asset')
    },
    error = function(e) {
      NULL
    })
    message(x)
    p
  })

  tmp2 = group_by(modis_collection, tile) %>%
    slice(1) %>%
    ungroup() %>%
    filter(complete.cases(.))

  modis_grid = lapply(1:nrow(tmp2), function(x) {
    nc = tryCatch({
      open.nc(tmp2$tmp[x])
    }, error = function(e) {
      NULL
    }, warning = function(w) {
      NULL
    })

    raw = if (!is.null(nc)) {
      tryCatch({
        raw  = opendap.catalog:::.resource_grid(nc, X_name = "XDim", Y_name = "YDim")
        raw$X_name = 'XDim'
        raw$Y_name = "YDim"
        raw$tile = tmp2$tile[x]
        raw
      }, error = function(e) {
        atts <- dap_xyzv(nc)

        if (nrow(atts) == 0) {
          NULL
        } else {
          X_name <- unique(atts$X_name)
          Y_name <- unique(atts$Y_name)

          tryCatch({
            raw  = opendap.catalog:::.resource_grid(nc, X_name = X_name, Y_name = Y_name)
            raw$X_name = X_name
            raw$Y_name = Y_name
            raw$tile = tmp2$tile[x]
            raw
          }, error = function(e) {
            NULL
          })
        }
      })

    } else {
      NULL
    }


    message(x)

    raw
  })

  modis_param2 = bind_rows(modis_param) |>
    mutate(
      URL = paste0(URL, asset),
      tiled = " XY",
      type = "opendap",
      description = long_name,
      variable = varname
    )

  modis_grid2 = bind_rows(modis_grid)

  left_join(
    select(bind_rows(modis_data), asset = id, tile),
    select(modis_param2, -X_name, -Y_name),
    by = c("asset")
  ) %>%
    left_join(modis_grid2, by = "tile") %>%
    mutate(
      crs = proj,
      tiled = "XY",
      type = "opendap",
      URL = paste0(URL, "/", tile, ".ncml")
    ) %>%
    filter(complete.cases(.)) %>%
    mutate(crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs') %>%
    rectify_schema(schema)
  }

# Data Source 25

get_livneh = function(id = "Livneh_daily"){

 dates = seq.Date(as.Date("1950-01-01"), as.Date("2013-12-31"), by = "day")

 df =  data.frame(id = id,
             URL = glue('https://www.ncei.noaa.gov/thredds-ocean/dodsC/ncei/archive/data/0129374/daily/livneh_NAmerExt_15Oct2014.{unique(format(dates, "%Y%m"))}.nc'),
             type = "opendap",
             ym = unique(format(dates, "%Y%m")),
             tiled = "T")

 tmp = data.frame(date = dates, ym = format(dates, "%Y%m")) %>%
   group_by(ym) %>%
   mutate(duration = paste0(date[1], "/", date[n()]), nT = n()) %>%
   slice(1) %>%
   select(ym, duration, nT)

 opendap.catalog::read_dap_file(df$URL[2], varname = NULL, id = id) %>%
   mutate(URL = NULL) %>%
   right_join(df, by = "id") %>%
   mutate(duration = NULL, nT = NULL, description = long_name, variable = varname, crs = proj) %>%
   left_join(tmp, by = "ym") %>%
   rectify_schema(schema)
}

get_livneh_monthly = function(id = "Livneh_monthly"){

  dates = data.frame(dates = seq.Date(as.Date("1950-01-01"), as.Date("2013-12-31"), by = "day") )%>%
    mutate(ym = format(dates, "%Y%m"),
           y =  format(dates, "%Y"))

  tmp = slice(group_by(dates, ym), 1)

  df =  data.frame(id = id,
                   URL = glue('https://www.ncei.noaa.gov/thredds-ocean/dodsC/ncei/archive/data/0129374/monthly/livneh_NAmerExt_15Oct2014.{tmp$ym}.mon.nc'),
                   type = "opendap",
                   ym = tmp$ym,
                   tiled = "T")

  tmp = dates %>%
    group_by(ym) %>%
    mutate(duration = paste0(dates[1], "/", dates[n()]), nT = 1) %>%
    slice(1) %>%
    select(ym, duration, nT)

  opendap.catalog::read_dap_file(df$URL[2], varname = NULL, id = id) %>%
    mutate(URL = NULL) %>%
    right_join(df, by = "id") %>%
    mutate(duration = NULL, nT = NULL, description = long_name, variable = varname, crs = proj) %>%
    left_join(tmp, by = "ym") %>%
    rectify_schema(schema)
}


# Data Source 25 -----------------------------------------------------------

get_livneh_fluxes = function(id = "Livneh_fluxes"){

  dates = data.frame(dates = seq.Date(as.Date("1950-01-01"), as.Date("2013-12-31"), by = "day") )%>%
    mutate(ym = format(dates, "%Y%m"),
           y =  format(dates, "%Y"))

  tmp = slice(group_by(dates, ym), 1)

  df =  data.frame(id = id,
                   URL = glue('https://www.ncei.noaa.gov/thredds-ocean/dodsC/livnehmodel/{tmp$y}/Fluxes_Livneh_NAmerExt_15Oct2014.{tmp$ym}.nc'),
                   type = "opendap",
                   ym = tmp$ym,
                   tiled = "T")

  tmp = dates %>%
    group_by(ym) %>%
    mutate(duration = paste0(dates[1], "/", dates[n()]), nT = n()) %>%
    slice(1) %>%
    select(ym, duration, nT)

  opendap.catalog::read_dap_file(df$URL[2], varname = NULL, id = id) %>%
    mutate(URL = NULL) %>%
    right_join(df, by = "id") %>%
    mutate(duration = NULL, nT = NULL, description = long_name, variable = varname, crs = proj) %>%
    left_join(tmp, by = "ym") %>%
    rectify_schema(schema)
}

# Data Source 26

get_lcmap = function(base = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/lcmap/public/full_extent_downloads") {


  df = data.frame(domain = c("CU", "HI"),
                  domain2 = c("conus", "hawaii"),
                  version = c("version_13", "version_10"),
                  version2 = c("V13", "V10"))
  year = 1985:2021
  meta= data.frame(asset = c('LCPRI','LCSEC','LCPCONF','LCSCONF','LCACHG','SCTIME','SCMAG','SCLAST','SCSTAB', 'SCMQA'),
  description = c('primary-landcover','secondary-landcover','primary-confidence','secondary-confidence','cover-change','change-day',
                  'change-magnitude','spectral-lastchange','spectral-stability','model-quality'))

  merge(df, year) %>%
    rename(year = y) %>%
    arrange(domain) %>%
    merge(meta) %>%
    mutate(URL = glue::glue('/vsicurl/{base}/{version}/{description}_{domain2}_year_data/LCMAP_{domain}_{year}_{version2}_{asset}/LCMAP_{domain}_{year}_{version2}_{asset}.tif'),
           id = "LCMAP",
           type = "vrt",
           varname = description,
           variable = description,
           description = paste(year, gsub("-", " ", description)),
           units = "",
           interval = "1 year",
           nT = 1,
           tiled = "T",
           duration = paste0(year, "-01-01/", year, "-12-31")) %>%
    vrt_meta(all = FALSE) %>%
    rectify_schema(schema)
}


# Data Source 27 ----------------------------------------------------------

get_prism_daily = function(){
  read_dap_file(URL = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2021/PRISM_combo_20211010.nc',
                id = 'prism_daily') %>%
    mutate(duration = '1981-01-01/..', variable = varname) %>%
    dap_meta() %>%
    mutate(URL =   gsub("20211010", "{YYYYMMDD}", gsub("/2021\\/", "/{YYYY}/", URL)), tiled = "T")
}

