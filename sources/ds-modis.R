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
    dplyr::filter(grepl("MOD", id)) |>
    dplyr::filter(id == "MOD14A1.006")

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
    dplyr::mutate(tmp = paste0(link, tile, ".ncml#fillmismatch"))

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

  tmp2 = dplyr::group_by(modis_collection, tile) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    tidyr::drop_na()

  modis_grid = lapply(1:nrow(tmp2), function(x) {
    nc = tryCatch({
      RNetCDF::open.nc(tmp2$tmp[x])
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
        atts <- climateR::dap_xyzv(nc)

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

  modis_param2 = dplyr::bind_rows(modis_param) |>
    dplyr::mutate(
      URL = paste0(URL, asset),
      tiled = " XY",
      type = "opendap",
      variable = varname
    )

  modis_grid2 = dplyr::bind_rows(modis_grid)

  dplyr::left_join(
    dplyr::select(dplyr::bind_rows(modis_data), asset = id, tile),
    dplyr::select(modis_param2, -X_name, -Y_name),
    by = c("asset")
  ) |>
    dplyr::left_join(modis_grid2, by = "tile") |>
    dplyr::mutate(
      crs = proj,
      tiled = "XY",
      type = "opendap",
      URL = paste0(URL, "/", tile, ".ncml")
    ) |>
    tidyr::drop_na() |> 
    dplyr::mutate(crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')
}

# .pull_modis <- function(...) {
#     url <- "https://opendap.cr.usgs.gov/opendap/hyrax/"
# 
#     modis_hyrax <-
#         dplyr::tibble(
#             link = rvest::read_html(url) |>
#                 rvest::html_nodes("a") |>
#                 rvest::html_attr("href")
#         ) |>
#         dplyr::mutate(
#             id = dirname(link),
#             link = paste0(url, gsub('contents.html', "", link))
#         ) |>
#         dplyr::filter(!grepl("http|4913|opendap|PROTOTYPE", id)) |>
#         dplyr::filter(id != ".") |>
#         dplyr::filter(grepl("MOD", id)) |>
#         dplyr::filter(id == "MOD14A1.006")
# 
#     modis_data <- list()
# 
#     for (i in seq_len(nrow(modis_hyrax))) {
#         modis_data[[i]] <- NULL
# 
#         urls <- tryCatch({
#                 rvest::read_html(modis_hyrax$link[i]) |>
#                     rvest::html_nodes("a") |>
#                     rvest::html_attr("href")
#             }, error = function(e) NULL)
# 
#         if (!is.null(urls)) {
#             modis_data[[i]] <-
#                 dplyr::tibble(link = urls) |>
#                 dplyr::filter(grepl(".ncml.dap", link)) |>
#                 dplyr::mutate(
#                     id = modis_hyrax$id[i],
#                     tile = gsub(".ncml.dap", "", link),
#                     link = modis_hyrax$link[i]
#                 )
#         }
# 
#         message(i, " of ", nrow(modis_hyrax))
#     }
# 
#     message("getting modis_collection")
#     modis_collection <-
#         dplyr::bind_rows(modis_data) |>
#         dplyr::filter(id != "") |>
#         dplyr::group_by(id) |>
#         dplyr::mutate(
#             mosaic = dplyr::n(),
#             tiled = dplyr::if_else(mosaic > 1, "XY_modis", ""),
#             tile  = dplyr::if_else(tile == "", id, tile)
#         ) |>
#         dplyr::ungroup() |>
#         dplyr::mutate(tmp = paste0(link, tile, ".ncml#fillmismatch"))
# 
#     tmp <- dplyr::group_by(modis_collection, id) |>
#            dplyr::slice(1) |>
#            dplyr::ungroup()
# 
#     message("getting modis_param")
#     modis_param <- lapply(seq_len(nrow(tmp)), function(x) {
#         tryCatch({
#             nc        <- RNetCDF::open.nc(tmp$tmp[x])
#             raw       <- climateR::dap_xyzv(obj = nc, varmeta = TRUE)
#             raw$id    <- "MODIS"
#             raw$asset <- tmp$id[x]
#             raw$tiled <- tmp$tiled[x]
# 
#             merge(
#                 x  = raw,
#                 y  = data.frame(climateR:::.resource_time(nc, raw$T_name[1]),
#                                 asset = tmp$id[x]),
#                 by = "asset"
#             )
#         }, error = function(e) NULL)
#     })
# 
#     tmp2 <- dplyr::group_by(modis_collection, tile) |>
#             dplyr::slice(1) |>
#             dplyr::ungroup() |>
#             tidyr::drop_na()
# 
#     message("getting modis_grid")
#     modis_grid <- lapply(seq_len(nrow(tmp)), function(x) {
#         message("modis grid ", x)
# 
#         nc <- tryCatch({
#             RNetCDF::open.nc(tmp2$tmp[x])
#         }, error = function(e) NULL, warning = function(w) NULL)
# 
#         raw <- NULL
# 
#         if (!is.null(nc)) {
#             raw <- tryCatch({
#                 r <- climateR:::.resource_grid(nc, X_name = "XDim", Y_name = "YDim")
#                 r$X_name <- "XDim"
#                 r$Y_name <- "YDim"
#                 r$tile   <- tmp2$tile[x]
#                 r
#             }, error = function(e) {
#                 atts <- climateR::dap_xyzv(nc)
# 
#                 if (nrow(atts) != 0) {
#                     X_name <- unique(atts$X_name)
#                     Y_name <- unique(atts$Y_name)
# 
#                     tryCatch({
#                         r <- climateR:::.resource_grid(nc, X_name = X_name, Y_name = Y_name)
#                         r$X_name <- X_name
#                         r$Y_name <- Y_name
#                         r$tile   <- tmp2$tile[x]
#                         r
#                     }, error = function(e) NULL)
#                 } else {
#                     NULL
#                 }
#             })
#         }
# 
#         raw
#     })
# 
#     modis_param2 <- dplyr::bind_rows(modis_param) |>
#                     dplyr::mutate(
#                         URL = paste0(url, asset),
#                         tiled = "XY",
#                         type = "opendap"
#                     ) |>
#                     dplyr::select(-X_name, -Y_name)
# 
#     modis_grid2 <- dplyr::bind_rows(modis_grid)
# 
#     modis_data2 <- dplyr::bind_rows(modis_data) |>
#                    dplyr::select(asset = id, tile)
# 
#     message("modis finishing")
# 
#     dplyr::left_join(modis_data2, modis_param2, by = "asset") |>
#         dplyr::left_join(modis_grid2, by = "tile") |>
#         dplyr::mutate(
#             tiled = "XY",
#             type = "opendap",
#             URL = paste0(URL, "/", tile, ".ncml")
#         ) |>
#         tidyr::drop_na() |>
#         dplyr::mutate(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") |>
#         arrow::as_arrow_table()
# }

# ---------------------------------------------------------------------

.tidy_modis <- function(.tbl, ...) .tbl

# ---------------------------------------------------------------------

ds_modis <- climateR.catalogs::data_source$new(
    id   = "modis",
    pull = .pull_modis,
    tidy = .tidy_modis
)
