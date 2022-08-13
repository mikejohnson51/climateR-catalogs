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
    df$collection = "HBV"
    df$varname = df$variable
    df$asset = strsplit(basename(terra::sources(r)), "[.]")[[1]][1]
    df$URL = terra::sources(r)
    df$type = "multiband_tif"

    data[[i]] = left_join(df, v, by = "variable") %>%
      select(collection, asset, URL, type, varname, variable, description, units)
  }

  bind_rows(data)
}

