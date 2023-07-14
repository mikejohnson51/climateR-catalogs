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


    climateR::dap_meta() %>%
    mutate(tiled = "T", type = "opendap") %>%
    rectify_schema(schema)


}

# Data Source 2 -----------------------------------------------------------
get_gridmet <- function(id = "gridmet"){
  o = read_tds("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html", id) %>%
    separate_wider_delim(link,
                  names = c(NA, NA, "variable", NA, NA, NA),
                  delim = "_")

  out = list()

  for(i in 1:nrow(o)){
    out[[i]] = tryCatch({
      climateR::read_dap_file(o$URL[i], id = o$variable[i], varmeta = TRUE)
    }, error = function(e) { NULL})
  }

  bind_rows(out) %>%
    rename(variable = id) %>%
    left_join(select(o, -URL), by = 'variable') %>%
    mutate(tiled = "", type = "opendap") %>%
    rectify_schema(schema)

}

# Data Source 3 -----------------------------------------------------------
get_terraclim = function(id = "terraclim"){
  read_tds("http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html", id) %>%
    separate_wider_delim(link, names = c(NA, NA, "variable", NA, NA, NA), delim = "_", too_few = "align_end") %>%
    climateR::dap_meta() %>%
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
  climateR::dap_meta() |>
  mutate(tiled = "", interval = "1 month", type = "opendap", crs = proj, description = long_name) %>%
  rectify_schema(schema)

}

# Data Source 5 -----------------------------------------------------------
# get_vic = function(id = "vic"){
#   d = read_tds(URL = "https://www.reacchpna.org/thredds/nw.csc.hydrology-vic.aggregated.html", id) %>%
#     separate_wider_delim(link,
#                          names = c(NA, "variable", "model", "ensemble", "scenario",
#                                    NA, NA, NA, NA, NA, NA),
#                          delim = "_",
#                          too_few = "align_start")
#
#   d2 = group_by(d, variable, scenario) %>%
#     slice(1) %>%
#     select(URL, variable, scenario)
#
#   d2$URL[4]
#   out = list()
#
#   for(i in 1:nrow(d2)){
#     out[[i]] = tryCatch({
#       climateR::read_dap_file(d2$URL[i], id = d2$variable[i], varmeta = TRUE)
#     }, error = function(e) { NULL})
#   }
#
#   bind_rows(out)
#
#   d2 = group_by(d, scenario) %>%
#     slice(1) %>%
#     select(URL, scenario)
#
#   out2 = list()
#   for(i in 1:nrow(d2)){
#     out[[i]] = tryCatch({
#       climateR::read_dap_file(d2$URL[i], id = d2$variable[i], varmeta = TRUE)
#     }, error = function(e) { NULL})
#   }
#
#   out = list()
#
#
#   bind_rows(out) %>%
#     rename(variable = id, description = long_name, crs = proj) %>%
#     left_join(select(o, -URL), by = 'variable') %>%
#     mutate(tiled = "", type = "opendap") %>%
#     rectify_schema(schema)
#
# }

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
        climateR::dap_meta() %>%
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
    ldas[[i]] = climateR::read_dap_file(paste0(URL[i], "/"), id = basename(URL[i]))
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
    climateR::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_historical", id = id),
    climateR::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_future", id = id)
  ) %>%
    separate_wider_delim(varname,
                    names = c("variable", "model", "ensemble", "scenario"),
                    delim = "_",
                    cols_remove = FALSE) %>%
    mutate(tiled = "T", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 10 -----------------------------------------------------------
get_bcca <- function(id = "bcca"){

  bind_rows(
    climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future", id = id),
    climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical", id = id)
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
  climateR::read_dap_file("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC", id = id) %>%
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


  raw = climateR::read_dap_file("https://cida.usgs.gov/thredds/dodsC/bcsd_obs", id = id) %>%
    climateR::dap_meta() %>%
    mutate(tiled = "", type = "opendap") %>%
    rectify_schema(schema)
}

# Data Source 13 -----------------------------------------------------------
get_dcp = function(id = "dcp"){
  climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/dcp/conus_t", id = id) %>%
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
  climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml", id = id) %>%
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
  climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly", id = id) %>%
    climateR::dap_meta() %>%
    mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 16 -----------------------------------------------------------
get_topowx = function(id = "ssebopeta"){
  bind_rows(
    climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/topowx", id = "topowx_daily"),
    climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/topowx_monthly", id = "topowx_monthly"),
    climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/topowx_normals", id = "topowx_normals")) %>%
    mutate(tiled = "", variable = varname, type = "opendap", crs = proj, description = long_name) %>%
    rectify_schema(schema)
}

# Data Source 17 -----------------------------------------------------------
get_prism_monthly = function(id = "prism_monthly"){
  climateR::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/prism_v2", id = id) %>%
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

  modis_hyrax = data.frame(link = html_attr(html_nodes(read_html(URL), "a"), "href")) |>
    mutate(id = dirname(link),
           link = paste0(URL,
                         gsub('contents.html', "", link))) |>
    filter(!grepl("http|4913|opendap|PROTOTYPE", id)) |>
    filter(id != ".") |>
    filter(grepl("MOD", id)) %>%
    filter(id == "MOD14A1.006")

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
              climateR:::.resource_time(nc, raw$T_name[1]),
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
        raw  = climateR:::.resource_grid(nc, X_name = "XDim", Y_name = "YDim")
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
            raw  = climateR:::.resource_grid(nc, X_name = X_name, Y_name = Y_name)
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

 climateR::read_dap_file(df$URL[2], varname = NULL, id = id) %>%
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

  climateR::read_dap_file(df$URL[2], varname = NULL, id = id) %>%
    mutate(URL = NULL) %>%
    right_join(df, by = "id") %>%
    mutate(duration = NULL, interval = "1 month", nT = NULL, description = long_name, variable = varname, crs = proj) %>%
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

  climateR::read_dap_file(df$URL[2], varname = NULL, id = id) %>%
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
    mutate(crs = proj, description = long_name, type = "opendap") %>%
    mutate(URL =   gsub("20211010", "{YYYYMMDD}", gsub("/2021\\/", "/{YYYY}/", URL)),
           tiled = "T",
           interval = "1 day") %>%
    rectify_schema(schema)
}

# Data Source 28 ----------------------------------------------------------

get_WUS_HSP = function(){
  climateR::read_dap_file("https://cida.usgs.gov/thredds/dodsC/WUS_HSP/SD_A1B_2040s",
                                    id = "WUS_HSP") %>%
 separate_wider_delim(varname,
                       names = c("junk", "scenario", "junk2", "model", "variable"),
                       delim = "_",
                       too_many = "merge",
                       cols_remove = FALSE) %>%
  mutate(tiled = "", type = "opendap", crs = proj, description = long_name) %>%
  rectify_schema(schema)
}


# Data Source 28 ----------------------------------------------------------

get_loca_hydro = function(){
  url <- "ftp://gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/loca_hydro/LOCA_VIC_dpierce_2017-02-28/"
  result <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)

  models = strsplit(result, "\\n")[[1]]

  url2 = paste0(url, models[1], "/")
  result2 <- getURL(url2,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
  scenarios = strsplit(result2, "\\n")[[1]]

  url3 = paste0(url2, "/", scenarios[1], "/")
  result3 <- getURL(url3,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
  tmp = strsplit(strsplit(result3, "\\n")[[1]], "\\.")
  meta = data.frame(do.call(rbind, tmp)) %>%
    filter(X2 != "nc")

  g1 = expand.grid(models, scenarios[1], unique(meta$X1), unique(readr::parse_number(meta$X2)))

  url3 = paste0(url2, "/", scenarios[2], "/")
  result4 <- getURL(url3,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
  tmp = strsplit(strsplit(result4, "\\n")[[1]], "\\.")
  meta = data.frame(do.call(rbind, tmp)) %>%
    filter(X2 != "nc")

  g2 = expand.grid(models, scenarios[2:3], unique(meta$X1), unique(readr::parse_number(meta$X2)))

  g3 = data.frame(rbind(g1, g2)) %>%
    distinct() %>%
    filter(complete.cases(.))

  names(g3) = c("model", "scenario", "varname", "year")

  x = group_by(g3, model, scenario, varname) %>%
    mutate(interval = "1 day", duration = paste0(min(year), "-01-01/", max(year), "-12-31"),
           id = "loca_hydrology",
           URL = paste0(url, model,"/", scenario, "/", varname, ".{year}.v0.nc"),
           scenario = gsub(".netcdf", "", gsub("vic_output.", "", scenario)),
           type = "ftp", variable = varname, description = varname, toptobottom = FALSE, tiled = "T", T_name = "Time",
           X_name = "Lon", Y_name = "Lat", nT = 365) %>%
    slice(1) %>%
    ungroup()


  x2 = group_by(x, varname) %>%
    slice(1) %>%
    mutate(URL = glue(URL)) %>%
    ungroup() %>%
    select(URL, varname)


  for(i in 1:nrow(x2)){
    r = rast(x2$URL[i], lyrs = 1)
    x2$units[i] = units(r)
    x2$crs[i] = sf::st_crs(r)$proj4string
    x2$X1[i] = xmin(r)
    x2$Xn[i] = xmax(r)
    x2$Y1[i] = ymin(r)
    x2$Yn[i] = ymax(r)
    x2$resX[i] = res(r)[1]
    x2$resY[i] = res(r)[2]
    x2$ncols[i] = ncol(r)
    x2$nrows[i] = nrow(r)
    message(i , " of ", nrow(x2))
  }

  left_join(x, select(x2, - URL), by = 'varname') %>%
    rectify_schema(schema)
}


# Data Source 29 ----------------------------------------------------------

get_merra = function( base = 'https://goldsmr5.gesdisc.eosdis.nasa.gov/opendap/MERRA2/'){

  das = html_nodes(read_html(base), "a")
  URL = unique(gsub("\\.[a-z]*$","", html_attr(das, "href")))
  assests = dirname(URL[grepl("/contents", URL)])

  merra1 = list()

  for(a in 1:length(assests)){

    g = expand.grid(assest = assests[a], year = 1980:2023,
                    month = sprintf("%02s", 1:12)) %>%
      mutate(ext = paste0(base, assest, "/", year, "/", month))

    x = read_tds(g$ext[1], id = g$Var1[1], append = "")

    x = filter(x, grepl(".nc4$", x$URL)) %>%
      mutate(URL = paste0('https://goldsmr5.gesdisc.eosdis.nasa.gov/opendap',link))

    var = read_dap_file(x$URL[1], id = "MERRA2")

    merra = list()

    for(i in 1:nrow(g)){

      dates = as.Date(paste(g$year[i],
                            g$month[i],
                            "01",
                            sep = "-"))

      files = gsub("19840101", '{date}', unique(basename(var$URL)))

      x2 = data.frame(
        date = seq.Date(dates, by = "day",
                        length.out = days_in_month(dates))) %>%
        mutate( date2 = gsub("-", "", date),
                duration = glue("{date} 01:30:00/{date} 22:30:00"),
                URL = glue(files, date = date2),
                tiled = "T"
        )

      merra[[i]] =  crossing(select(x2, -date, -date2),
                             select(var, -URL, -duration)) %>%
        mutate(URL = paste0(dirname(g$ext[i]), URL))

    }

    merra1[[a]] = data.table::rbindlist(merra) %>%
      mutate(variable  = varname,
             type = "opendap") %>%
      rectify_schema(schema) %>%
      mutate(asset = assests[a])

    message("\tFinished [", assests[a], "] (", a, " of ", length(assests), ")" )
  }


  rbindlist(merra1)
}


# Data Source 30 ----------------------------------------------------------
