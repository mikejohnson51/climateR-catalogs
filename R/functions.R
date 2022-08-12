elevation_data = function(start){
    df = data.frame(
    source  = c("USGS 3DEP", "USGS 3DEP", " USGS 3DEP", "GEBCO2019", "NASADEM"),
    product = c("30m CONUS DEM", "10m CONUS DEM", "60m Alaska DEM", "Global DEM", "Global DEM"),
    URL = c(
      '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt',
      '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt',
      '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/2/TIFF/USGS_Seamless_DEM_2.vrt',
      '/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2019_GEOTIFF/GEBCO_2019.tif',
      '/vsicurl/https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt'
    ),
    units = rep("meters", 5)
  ) %>%
      mutate(description = product)

 return(df)
}


vrt_meta = function(data, all = TRUE){

  if(all){
    for(i in 1:nrow(data)){
      r   = terra::rast(data$URL[i])
      data$X1[i]      = terra::xmin(r)
      data$Xn[i]      = terra::xmax(r)
      data$Y1[i]      = terra::xmin(r)
      data$Yn[i]      = terra::ymax(r)
      data$resX[i]    = terra::xres(r)
      data$resY[i]    = terra::yres(r)
      data$ncols[i]   = terra::ncol(r)
      data$nrows[i]   = terra::nrow(r)
      data$proj[i]    = sf::st_crs(r)$proj4string
    }
  } else {
    r   = terra::rast(data$URL[1])
    data$X1     = terra::xmin(r)
    data$Xn     = terra::xmax(r)
    data$Y1     = terra::xmin(r)
    data$Yn     = terra::ymax(r)
    data$resX    = terra::xres(r)
    data$resY    = terra::yres(r)
    data$ncols   = terra::ncol(r)
    data$nrows  = terra::nrow(r)
    data$proj  = sf::st_crs(r)$proj4string
  }


  data
}

polaris_urls = function(base){

  ids = base |>
    read_html() |>
    html_nodes("a") |>
    html_attr("href")

  ids = grep('vrt$',ids, value = TRUE)

  polaris_describe( data.frame(URL = glue("/vsicurl/{base}{ids}")))

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
      setNames(c('product', "description", "units"))

    v$source = "polaris"

    m = strsplit(x, "_")[[1]]

    if(length(m) == 5){
      m = c(paste0(m[1], "_", m[2]), m[3], m[4], m[5])
    }

    v = v[v$product == m[1], ]

    v$product = paste0(m[2]," ", m[1], " ", m[3], "-", m[4],  'cm' )

    v
  }


  ids = gsub(".vrt", "", basename(data$URL))

  out = lapply(ids, parse_polaris_description) %>%
    dplyr::bind_rows()

  cbind(data, out)

}

nlcd_urls = function(base = "https://storage.googleapis.com/feddata-r/nlcd/"){

  year     = c(2019, 2016, 2011, 2008, 2006, 2004, 2001)
  dataset  = c('Land_Cover', 'Impervious', 'Tree_Canopy')
  landmass = c('L48', 'AK', 'HI', 'PR')

  g = expand.grid(year, dataset, landmass)

  file <- paste0(g$Var1, "_", g$Var2, "_", g$Var3, ".tif")

  df = data.frame(
    URL = paste0(base, file),
    product = paste("USGS NLCD", gsub("_", " ", g$Var2)),
    description = paste("NLCD", g$Var2, g$Var3,  g$Var1)
  )

  for(i in 1:nrow(df)){
    df$exists[i] =  df$URL[i] %>%
      httr::HEAD() %>%
      httr::status_code() %>%
      identical(200L)
  }

  filter(df, exists) %>%
    mutate(URL = paste0("/vsicurl/", URL)) %>%
    select(-exists)
}

create_catalog = function(x){
  dplyr::bind_rows(x)
}


export_catalog = function(x, path){
  jsonlite::write_json(x, path, pretty = TRUE)
  path
}
