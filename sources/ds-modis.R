.pull_modis <- function(...) {
    URL <- 'https://opendap.cr.usgs.gov/opendap/hyrax/'

  modis_hyrax = data.frame(link = rvest::html_attr(
    rvest::html_nodes(
        rvest::read_html(URL), "a"),
        "href"
  )) |>
    dplyr::mutate(id = dirname(link),
           link = paste0(URL,
                         gsub('contents.html', "", link))) |>
    dplyr::filter(!grepl("http|4913|opendap|PROTOTYPE", id)) |>
    dplyr::filter(id != ".") |>
    dplyr::filter(grepl("MOD", id))

  modis_data = list()

  for (i in 1:nrow(modis_hyrax)) {
    tmp = tryCatch({
      data.frame(link = rvest::html_attr(rvest::html_nodes(rvest::read_html(
        modis_hyrax$link[i]
      ), "a"), "href"))
    }, error = function(e) {
      NULL
    })

    if (!is.null(tmp)) {
      modis_data[[i]]  = data.frame(link = rvest::html_attr(rvest::html_nodes(rvest::read_html(
        modis_hyrax$link[i]
      ), "a"), "href")) |>
        dplyr::filter(grepl(".ncml.dap", link)) |>
        dplyr::mutate(
          id = modis_hyrax$id[i],
          tile = gsub('.ncml.dap', "", link),
          link = modis_hyrax$link[i]
        )
    } else {
      modis_data[[i]] = NULL
    }

    message(i, " of ", nrow(modis_hyrax))
  }

  modis_collection = dplyr::bind_rows(modis_data) |>
    dplyr::filter(id != "") |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      mosaic = dplyr::n(),
      tiled = ifelse(mosaic > 1, "XY_modis", ""),
      tile  = ifelse(tile == "", id, tile)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(tmp = paste0(link, tile, ".ncml"))

  tmp = dplyr::group_by(modis_collection, id) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  modis_param = lapply(1:nrow(tmp), function(x) {
    p = tryCatch({
      nc =  RNetCDF::open.nc(tmp$tmp[x])
      raw = climateR::dap_xyzv(obj = nc, varmeta = TRUE)
      raw$id = "MODIS"
      raw$asset  = tmp$id[x]
      raw$tiled = tmp$tiled[x]

      merge(raw,
            data.frame(
              climateR::.resource_time(tmp$tmp[x], raw$T_name[1]),
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

  tmp2 = dplyr::group_by(modis_collection, tile) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    tidyr::drop_na()

  modis_grid = lapply(1:nrow(tmp2), function(x) {
    raw =
      tryCatch({
        raw  = climateR:::.resource_grid(tmp2$tmp[x],
                                         X_name = "XDim",
                                         Y_name = "YDim")
        raw$X_name = 'XDim'
        raw$Y_name = "YDim"
        raw$tile = tmp2$tile[x]
        raw
      }, error = function(e) {
          tryCatch({
            atts = climateR::dap_xyzv(tmp2$tmp[x])
            raw  = suppressWarnings({
              climateR:::.resource_grid(tmp2$tmp[x],
                                        X_name = atts$X_name[1],
                                        Y_name = atts$Y_name[1])
            })

            raw$X_name = atts$X_name[1]
            raw$Y_name = atts$Y_name[1]
            raw$tile = tmp2$tile[x]
            raw
          }, error = function(e) {
            NULL
          })
      })

    message(x)

    raw
  })

  modis_param2 = dplyr::bind_rows(modis_param) |>
    dplyr::mutate(
      URL = paste0(URL, asset),
      tiled = " XY",
      type = "opendap",
      variable = varname
    )

  modis_grid2 = dplyr::bind_rows(modis_grid)

  .tbl = dplyr::left_join(
    dplyr::select(dplyr::bind_rows(modis_data), asset = id, tile),
    dplyr::select(modis_param2, -X_name, -Y_name),
    by = c("asset"),
    relationship = "many-to-many"
  ) |>
    dplyr::left_join(modis_grid2, by = "tile") |>
    dplyr::mutate(
      tiled = "XY",
      type = "opendap",
      URL = paste0(URL, "/", tile, ".ncml")
    ) |>
    #tidyr::drop_na() |>
    dplyr::mutate(crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs') |>
    arrow::as_arrow_table()

  return(.tbl)
}

# ---------------------------------------------------------------------

.tidy_modis <- function(.tbl, ...) .tbl

# ---------------------------------------------------------------------

ds_modis <- climateR.catalogs::data_source$new(
    id   = "modis",
    pull = .pull_modis,
    tidy = .tidy_modis
)
