
library(targets)
source("R/functions.R")
source("R/tds_functions.R")
tar_option_set(packages = c("terra", "sf", "rvest", "glue", "dplyr"))
# list(
#   #tar_target(elevation_vrts, elevation_data),
#   tar_target(file, "elevation.csv", format = "file"),
#   tar_target(elevation_vrts, get_data(file)),
#   tar_target(elevation, vrts_meta(elevation_vrts))
# )


list(
  # Elevation
  tar_target(elevation_vrts, elevation_data(1)),
  tar_target(elevation, vrt_meta(elevation_vrts)),

  # Soils
  tar_target(soils_vrts, polaris_urls('http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/')),
  tar_target(soils, vrt_meta(soils_vrts, all = FALSE)),

  ## Land cover
  tar_target(lc_vrts, nlcd_urls("https://storage.googleapis.com/feddata-r/nlcd/")),
  tar_target(lc, vrt_meta(lc_vrts)),

  tar_target(loca, get_loca()),
  tar_target(bcca, get_bcca()),

  tar_target(cat, create_catalog(list(elevation,
                                      soils,
                                      lc,
                                      loca,
                                      bcca))),

  tar_target(output, export_catalog(cat, "docs/catalog.json"), format = "file")

)


