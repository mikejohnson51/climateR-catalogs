library(targets)
library(tarchetypes)

source("R/functions.R")
source("R/tds_functions.R")

tar_option_set(packages = c("terra", "sf", "rvest", "glue", "dplyr"))

list(
  # Elevation
  tar_target(elevation_vrts, elevation_data(1)),
  tar_target(elevation, vrt_meta(elevation_vrts)),

  # Soils
  tar_target(polaris_vrts, polaris_urls()),
  tar_target(polaris, vrt_meta(polaris_vrts, all = FALSE)),
  tar_target(isric_vrts, isric_urls()),
  tar_target(isric, vrt_meta(isric_vrts, all = FALSE)),

  ## Land cover
  tar_target(nlcd_vrts, nlcd_urls()),
  tar_target(nlcd, vrt_meta(nlcd_vrts)),

  tar_target(loca, get_loca()),
  tar_target(bcca, get_bcca()),
  tar_target(bcsd_vic, get_bcsd_vic()),
  tar_target(bcsd, get_bcsd()),
  tar_target(dcp, get_dcp()),
  tar_target(maurer, get_maurer()),
  tar_target(ssebopeta, get_ssebopeta()),
  tar_target(prism_monthly, get_prism_monthly()),

  tar_target(cat, export_catalog(list(elevation,
                                      polaris,
                                      isric,
                                      nlcd,
                                      loca,
                                      bcca,
                                      bcsd_vic,
                                      bcsd, dcp,
                                      maurer,
                                      ssebopeta,
                                      prism_monthly),
                                 "docs/catalog.json"),
             format = "file"),

  tar_render(readme, "README.Rmd"),
  tar_render(schema, "docs/schema.Rmd")

)



