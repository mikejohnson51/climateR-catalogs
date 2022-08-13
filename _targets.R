library(targets)
library(tarchetypes)

source("R/functions.R")
source("R/tds_functions.R")
source("R/tds_mes.R")
source("R/hbv.R")

tar_option_set(packages = c("terra", "sf", "rvest", "glue", "dplyr"))

list(
  # Elevation (NASA, Australia, 3DEP)
  tar_target(elevation_vrts, elevation_data(1)),
  tar_target(elevation, vrt_meta(elevation_vrts)),

  # Soils (Ploaris and ISRIC)
  tar_target(polaris_vrts, polaris_urls()),
  tar_target(polaris, vrt_meta(polaris_vrts, all = FALSE)),
  tar_target(isric_vrts, isric_urls()),
  tar_target(isric, vrt_meta(isric_vrts, all = FALSE)),

  ## NLCD
  tar_target(nlcd_vrts, nlcd_urls()),
  tar_target(nlcd, vrt_meta(nlcd_vrts)),

  ## CIDA
  tar_target(loca, get_loca()),
  tar_target(bcca, get_bcca()),
  tar_target(bcsd_vic, get_bcsd_vic()),
  tar_target(bcsd, get_bcsd()),
  tar_target(dcp, get_dcp()),
  tar_target(maurer, get_maurer()),
  tar_target(ssebopeta, get_ssebopeta()),
  tar_target(prism_monthly, get_prism_monthly()),

  # TerraClim, GridMET and MACA
  tar_target(maca, get_maca()),
  tar_target(gridmet, get_gridmet()),
  tar_target(terraclim, get_terraclim()),
  tar_target(terraclim_normals, get_terraclim_normals()),
  tar_target(vic, get_vic()),

  # HBV
  tar_target(hbv_vrts, get_hbv()),
  tar_target(hbv, vrt_meta(hbv_vrts, all = FALSE)),

  # Export Catalog to catalog.json in doc directory
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
                                      prism_monthly,
                                      maca,
                                      gridmet,
                                      terraclim,
                                      terraclim_normals,
                                      vic,
                                      hbv
                                      ),
                                 "docs/catalog.json"),
             format = "file"),

  tar_render(readme, "README.Rmd"),
  tar_render(schema, "docs/schema.Rmd")

)



