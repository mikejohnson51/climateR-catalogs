get_loca = function(id = "loca"){
  bind_rows(
    opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_historical",
                                   id = id),
    opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_future",
                                   id = id)
  ) %>%
    tidyr::separate(varname,
                    into = c("variable", "model", "ensemble", "scenario"),
                    sep = "_",
                    remove = FALSE) %>%
    mutate(tiled = "T")
}

get_bcca <- function(id = "bcca"){

  bind_rows(
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future",
                                   id = id),
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical",
                                   id = id)
) %>%
  tidyr::separate(varname,
                  into = c(NA, NA, "variable", NA, "model", "scenario", "ensemble"),
                  sep = "_",
                  remove = FALSE) %>%
  opendap.catalog::dap_meta() %>%
  mutate(tiled = "T")
}

get_bcsd_vic <- function(id = 'bcsd_vic'){
  opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC",
                                 id = id) %>%
    tidyr::separate(varname,
                    into = c("model", "scenario", "ensemble", "variable"),
                    sep = "_", extra = "merge",
                    remove = FALSE) %>%
    opendap.catalog::dap_meta() %>%
    mutate(tiled = "")
}

get_bcsd = function(id = "bcsd"){
  opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/bcsd_obs",
                                 id = id) %>%
    opendap.catalog::dap_meta() %>%
  mutate(tiled = "")
}

get_dcp = function(id = "dcp"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/dcp/conus_t",
                id = id) %>%
  tidyr::separate(varname,
                  into = c("model", "scenario", "variable", NA, NA),
                  sep = "-",
                  extra = "merge",
                  remove = FALSE) %>%
  opendap.catalog::dap_meta() %>%
  mutate(tiled = "")
}

get_maurer = function(id = "maurer"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml",
                                 id = id) %>%
  tidyr::separate(varname,
                  into = c("scenario", "model", "ensemble", "variable"),
                  sep = "_",
                  extra = "merge",
                  remove = FALSE) %>%
  opendap.catalog::dap_meta() %>%
  mutate(tiled = "")
}

get_ssebopeta = function(id = "ssebopeta"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly",
                id = id) %>%
  opendap.catalog::dap_meta() %>%
  mutate(tiled = "")
}

get_prism_monthly = function(id = "prism_monthly"){
  opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/prism_v2",
                id = id) %>%
  mutate(variable = varname) %>%
  opendap.catalog::dap_meta() %>%
  mutate(tiled = "")
}

