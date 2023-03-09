library(targets)

source("R/utils.R")
source("R/targets.R")

pacman::p_load("terra", "sf", "rvest", "glue", "dplyr", "jsonlite",
               "logger", "rvest", "climateR","RNetCDF", "data.table", "tidyr")
tar_option_set(packages = c("terra", "sf", "rvest", "glue", "dplyr",
                            "jsonlite", "logger", "rvest", "climateR",
                            "RNetCDF", "data.table", "tidyr", "RCurl"))


list(
  tar_target(maca,                get_maca()),
  tar_target(gridmet,             get_gridmet()),
  tar_target(terraclim,           get_terraclim()),
  tar_target(terraclim_normals,   get_terraclim_normals()),
  #tar_target(vic,                 get_vic()),
  tar_target(daymet4,             get_daymet4()),
  tar_target(ldas,                get_ldas()),
  tar_target(worldclim_hist,      get_worldclim_hist()),
  tar_target(loca,                get_loca()),
  tar_target(bcca,                get_bcca()),
  tar_target(bcsd_vic,            get_bcsd_vic()),
  tar_target(bcsd,                get_bcsd()),
  tar_target(dcp,                 get_dcp()),
  tar_target(maurer,              get_maurer()),
  tar_target(ssebopeta,           get_ssebopeta()),
  tar_target(prism_monthly,       get_prism_monthly()),
  tar_target(prism_daily,         get_prism_daily()),
  tar_target(hbv,                 get_hbv()),
  tar_target(elevation,           get_elevation_data()),
  tar_target(polaris,             get_polaris()),
  tar_target(nlcd,                get_nlcd()),
  tar_target(lcmap,               get_lcmap()),
  tar_target(isric,               get_isric()),
  tar_target(erdap,               get_erdap()),
  tar_target(modis,               get_modis()),
  tar_target(topowx,              get_topowx()),
  tar_target(livneh_daily,        get_livneh()),
  tar_target(livneh_monthly,      get_livneh_monthly()),
  tar_target(livneh_daily_fluxes, get_livneh_fluxes()),
  tar_target(WUS_HSP,             get_WUS_HSP()),
  tar_target(loca_hydro,          get_loca_hydro()),

  # Export Catalog to catalog.json in doc directory
  tar_target(cat, export_catalog(list(maca,
                                      gridmet,
                                      terraclim,
                                      terraclim_normals,
                                      #vic,
                                      daymet4,
                                      ldas,
                                      worldclim_hist,
                                      loca,
                                      bcca,
                                      bcsd_vic,
                                      bcsd,
                                      dcp,
                                      maurer,
                                      ssebopeta,
                                      prism_monthly,
                                      prism_daily,
                                      hbv,
                                      elevation,
                                      polaris,
                                      nlcd,
                                      lcmap,
                                      isric,
                                      erdap,
                                      modis,
                                      topowx,
                                      livneh_daily,
                                      livneh_monthly,
                                      livneh_daily_fluxes,
                                      WUS_HSP,
                                      loca_hydro),
                                 "docs/catalog"),
             format = "file")

  # tar_render(readme, "README.Rmd"),
  # tar_render(schema, "docs/schema.Rmd")
)




