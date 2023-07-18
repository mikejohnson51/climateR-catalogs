.make_cache <- function(server, cache_dir = "data-raw", overwrite = FALSE) {
    cache_path <- file.path(cache_dir, paste0(server$short_name, ".rds"))

    links <- NULL
    if (!file.exists(cache_path) || overwrite) {
        message("not found or overwrite: ", cache_path)
        links <- tryCatch({
            httr::GET(server$url, httr::timeout(30)) |>
                rvest::read_html() |>
                rvest::html_nodes("a") |>
                rvest::html_attr("href")
        }, error = function(condition) NULL)
    }

    if (!is.null(links)) {
        message("pulling links")
        ds <- data.frame(link = links) |>
              dplyr::filter(grepl("html$", link)) |>
              dplyr::filter(grepl("griddap", link)) |>
              dplyr::filter(!grepl("test", link)) |>
              dplyr::mutate(link = gsub(".html", "", link),
                            id = basename(link)) |>
              dplyr::filter(id != "documentation")

        if (nrow(ds) > 0) {
            message("reading DAP from ", nrow(ds), " rows")
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
        message("returning ", cache_path)
        return(cache_path)
    } else {
        message("returning NA")
        return(NA)
    }
}


.pull_erdap <- function(cache_dir = "data-raw", ...) {
    if (!dir.exists(cache_dir)) {
        stop(cache_dir, " does not exist")
    }

    cache_dir <- normalizePath(cache_dir, mustWork = TRUE)

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

    browser()

    message("creating ERDAP table")

    Filter(function(path) !is.na(path) && file.exists(path), paths) |>
        lapply(readRDS) |>
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
