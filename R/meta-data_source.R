#' Create a new climateR.catalogs data source plugin
#' @param name Name of the data source
#' @param dir Directory to output
#' @export
new_data_source <- function(name, dir) {
    output_path <- file.path(dir, paste0("ds-", name, ".R"))

    if (file.exists(output_path)) {
        stop("file ", output_path, " already exists!")
    }

    output <- glue::glue("
.pull_{name} <- function(...) {{
    # TODO: Implementation here
}}

# ---------------------------------------------------------------------

.tidy_{name} <- function(.tbl, ...) {{
    # TODO: Implementation here
}}

# ---------------------------------------------------------------------

ds_{name} <- climateR.catalogs::data_source$new(
    id   = \"{name}\",
    pull = .pull_{name},
    tidy = .tidy_{name}
)
    ")

    message("Creating new data source `",
            name, "` at ", output_path)

    writeLines(output, output_path)

    invisible(output_path)
}
