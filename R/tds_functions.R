get_loca = function(id = "loca"){
  bind_rows(
    opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_historical", id = id),
    opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_future", id = id)
  ) %>%
    tidyr::separate(varname, into = c("variable", "model", "ensemble", "scenario"), sep = "_", remove = FALSE) %>%
    mutate(tiled = "T")
}


get_bcca <- function(id = "bcca"){

  bind_rows(
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future", id = id),
    opendap.catalog::read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical", id = id)
) %>%
  tidyr::separate(varname, into = c(NA, NA, "variable", NA, "model", "scenario", "ensemble"), sep = "_", remove = FALSE) %>%
  opendap.catalog::dap_meta() %>%
  mutate(tiled = "T")
}

get_bcsd_vic <- function(id = 'bcsd_vic'){
  opendap.catalog::read_dap_file("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC", id = "bcsd_vic") %>%
    tidyr::separate(varname, into = c("model", "scenario", "ensemble", "variable"), sep = "_", extra = "merge") %>%
    opendap.catalog::dap_meta() %>%
    mutate(tiled = "")
}


