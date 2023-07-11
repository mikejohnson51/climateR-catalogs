get_griddap_server <- function(server, overwrite = FALSE) {
    outfile <- paste0("data-raw/", server$short_name, ".rds")

    if (any(!file.exists(outfile), overwrite)) {
      log_info("Checking Server: ", server$short_name)
      serv <- tryCatch(
        {
          html_attr(html_nodes(read_html(server$url), "a"), "href")
        },
        error = function(e) {
          NULL
        }
      )


      if (!is.null(serv)) {
        log_info("Scrapping Server: ", server$short_name)
        ds <- data.frame(link = serv) |>
          filter(grepl("html$", link)) |>
          filter(grepl("griddap", link)) |>
          filter(!grepl("test", link)) |>
          mutate(link = gsub(".html", "", link), id = basename(link)) |>
          filter(id != "documentation")

        log_info(nrow(ds), " datasets found...")

        if (nrow(ds) > 0) {
          ds <- mutate(ds, n = 1:n(), mx = max(n))

          collection <- lapply(1:nrow(ds), function(x) {
            .internal_get(ds = ds[x, ])
          })

          saveRDS(bind_rows(collection), outfile)
        }
      } else {
        logger::log_info("No data found on: ", server$short_name)
      }
    }
  }


.make_cache <- function(server, cache_dir = "data-raw", overwrite = FALSE) {
    cache_path <- file.path(cache_dir, paste0(server$short_name, ".rds"))

    links <- NULL
    if (!file.exists(cache_path) || overwrite) {
        links <- tryCatch({
            rvest::read_html(server$url) |>
                rvest::html_nodes("a") |>
                rvest::html_attr("href")
        }, error = function(condition) NULL)
    }

    if (!is.null(links)) {
        ds <- data.frame(link = links) |>
              dplyr::filter(grepl("html$", link)) |>
              dplyr::filter(grepl("griddap", link)) |>
              dplyr::filter(!grepl("test", link)) |>
              dplyr::mutate(link = gsub(".html", "", link),
                            id = basename(link)) |>
              dplyr::filter(id != "documentation")

        if (nrow(ds) > 0) {
            ds <- dplyr::mutate(ds, n = seq_len(dplyr::n()), mx = max(n))

            collection <- lapply(seq_len(nrow(ds)), function(i) {
                message(paste0(i, "/", nrow(ds)))
                tryCatch({
                    suppressMessages({
                        climateR::read_dap_file(URL = ds$link, id = ds$id) |>
                            dplyr::mutate(variable = varname, tiled = "")
                    })
                }, error = function(condition) NULL)
            })

            dplyr::bind_rows(collection) |>
                saveRDS(cache_path)
        }
    }
}


.pull_erdap <- function(...) {
    servers <-
        "https://irishmarineinstitute.github.io/awesome-erddap/erddaps.json" |>
        jsonlite::read_json(simplifyVector = TRUE) |>
        dplyr::filter(public) |>
        dplyr::mutate(url = gsub("/erddap/index.html", "/erddap/", url)) |>
        dplyr::mutate(url = dplyr::if_else(
            substr(url, nchar(url), nchar(url)) == "/",
            url,
            paste0(url, "/")
        )) |>
        dplyr::mutate(url = paste0(url, "griddap/index.html"))
}

.tidy_erdap <- function(.tbl, ...) {}


ds_erdap <- climateR.catalogs::data_source$new(
    id   = "erdap",
    pull = .pull_erdap,
    tidy = .tidy_erdap
)
