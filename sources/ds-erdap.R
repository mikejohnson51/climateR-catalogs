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
                        climateR::read_dap_file(
                            URL = ds$link[i],
                            id = ds$id[i]
                        ) |>
                            dplyr::mutate(variable = varname, tiled = "")
                    })
                }, error = function(condition) NULL)
            })

            dplyr::bind_rows(collection) |>
                saveRDS(cache_path)
        }
    }

    if (file.exists(cache_path)) {
        return(cache_path)
    } else {
        return(NULL)
    }
}


.pull_erdap <- function(cache_dir = "data-raw", ...) {
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

    # Generate .RDS files
    paths <- lapply(seq_len(nrow(servers)), function(i) {
        tryCatch({
            .make_cache(servers[i, ], cache_dir)
        }, error = function(x) NULL)
    })

    lapply(paths[!sapply(paths, is.null)], readRDS) |>
        dplyr::bind_rows() |>
        arrow::as_arrow_table()
}

.tidy_erdap <- function(.tbl, ...) {
    dplyr::as_tibble(.tbl) |>
      dplyr::mutate(type = "erddap") |>
      arrow::as_arrow_table()
}


ds_erdap <- climateR.catalogs::data_source$new(
    id   = "erdap",
    pull = .pull_erdap,
    tidy = .tidy_erdap
)
