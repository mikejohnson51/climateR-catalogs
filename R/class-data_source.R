#' @title climateR Catalog Data Source
#'
#' @description
#' This is a generic base class for describing
#' catalog data sources. Each data source must
#' provide a `pull` and `tidy` method.
#'
#' @export
data_source <- R6::R6Class("data_source",
    lock_class = TRUE,

    public = list(
        #' @description
        #' Create a new catalog data source.
        #'
        #' @note If all parameters are missing, then
        #'  an empty data_source is created. This is
        #'  only useful for reading from IPC.
        #'
        #' @param id (`character(1)`)\cr
        #'  Identifier for this data source.
        #'
        #' @param pull (`function`)\cr
        #'  Pull method for this class.\cr
        #'
        #'  The pull function may require any amount of parameters.\cr
        #'
        #'  The pull function must return one of:
        #'      `arrow::Table`, `data.table::data.table`, or a `data.frame`.
        #'
        #' @param tidy (`function`)\cr
        #'  Tidy method for this class. See the `tidy` method for details.\cr
        #'
        #'  The tidy function must require at least 1 argument as its
        #'  first argument that takes in, and also **returns**, one of:
        #'      `arrow::Table`, `data.table::data.table`, or a `data.frame`
        #'
        initialize = function(id, pull, tidy) {
            if (!(missing(id) && missing(pull) && missing(tidy))) {
                stopifnot(id != "", is.function(pull), is.function(tidy))
                private$.id   <- id
                private$.pull <- pull
                private$.tidy <- tidy
            }
        },

        print = function(...) {
            is_pulled <- if (is.null(private$.data)) {
                glue::glue_col("{red \u2716}")
            } else {
                glue::glue_col("{green \u2714}")
            }

            is_tidied <- if (!private$.finished) {
                glue::glue_col("{red \u2716}")
            } else {
                glue::glue_col("{green \u2714}")
            }

            cat(glue::glue(
                "<data source> [{private$.id}]",
                "- Pulled ($pull): {is_pulled}",
                "- Tidied ($tidy): {is_tidied}",
                .sep = "\n",
                .literal = TRUE
            ))

            invisible(self)
        },

        #' @description
        #' Pull a catalog data source from its endpoint.
        #'
        #' This method is user-defined at object creation.
        #'
        #' @param ... (`any`)\cr
        #'  User-defined parameters that may be used.
        pull = function(..., ..attempts) {
            if (missing(..attempts)) {
                ..attempts <- 1
            } else {
                ..attempts <- ..attempts + 1
            }

            tryCatch({
                private$.data <- private$.pull(...) |>
                                arrow::as_arrow_table()
            }, error = function(condition) {
                if (..attempts > 2) {
                    targets::tar_throw_run(paste0(
                        private$.id,
                        ": failed after 3 attempts.\n  "
                    ), condition)
                } else {
                    targets::tar_warn_run(paste0(
                        private$.id,
                        ": failed attempt ", ..attempts, "\n  "
                    ), condition)
                    self$pull(..., ..attempts = ..attempts)
                }
            })

            invisible(self)
        },

        #' @description
        #' Tidy a raw catalog data source into the catalog schema.
        #'
        #' @param ... (`any`)\cr
        #'  User-defined parameters that may be used.
        tidy = function(..., ..attempts) {
            if (missing(..attempts)) {
                ..attempts <- 1
            } else {
                ..attempts <- ..attempts + 1
            }

            tryCatch({
                private$.data <- private$.tidy(private$.data, ...) |>
                                 arrow::as_arrow_table()
            }, error = function(condition) {
                if (..attempts > 2) {
                    targets::tar_throw_run(paste0(
                        private$.id,
                        ": failed after 3 attempts.\n  "
                    ), condition)
                } else {
                    targets::tar_warn_run(paste0(
                        private$.id,
                        ": failed attempt ", ..attempts, "\n  "
                    ), condition)
                    self$tidy(..., ..attempts = ..attempts)
                }
            })

            private$.finished <- TRUE

            invisible(self)
        },

        #' @description
        #' Marshal this data source to Arrow IPC Stream format
        #'
        #' @returns (`raw()`)\cr
        #'  The data member of this data source in Arrow IPC Stream format
        to_ipc_stream = function() {
            private$.write_metadata()
            arrow::write_to_raw(private$.data, format = "stream")
        },

        #' @description
        #' Unmarshals an Arrow IPC Stream to a data source
        #'
        #' @param stream (`raw()`)\cr
        #'  The given Arrow IPC Stream
        from_ipc_stream = function(stream) {
            private$.data <- arrow::read_ipc_stream(
                stream,
                as_data_frame = FALSE
            )

            private$.read_metadata()
            invisible(self)
        },

        #' @description
        #' Output this data source to Arrow IPC File format
        #'
        #' @param path (`character(1)`)\cr
        #'  Path to file to write to. Should have extension '.arrow'.
        #'
        #' @returns (`character(1)`)\cr
        #'  The given path, invisibly.
        to_ipc_file = function(path) {
            private$.write_metadata()
            arrow::write_ipc_file(private$.data, path, compression = "default")
            invisible(path)
        },

        #' @description
        #' Read a data source from Arrow IPC File format
        #'
        #' @param path (`character(1)`)\cr
        #'  Path to Arrow IPC file.
        from_ipc_file = function(path) {
            private$.data <- arrow::read_ipc_file(path, as_data_frame = FALSE)
            private$.read_metadata()
            invisible(self)
        }
    ),

    active = list(
        #' @field id (`character(1)`)\cr
        #'  Identifier of this data source.
        id = function(value) {
            if (missing(value)) {
                private$.id
            } else {
                stopifnot(id != "")
                private$.id <- value
                self
            }
        },

        #' @field result (`arrow::Table`)\cr
        #'  Result of this data source after `$tidy()` is called.
        #' (**Read-only**)
        result = function(value) {
            if (!missing(value)) {
                stop("`$result` is read-only.", call. = FALSE)
            }

            return(private$.data)
        }
    ),

    private = list(
        #' @field .id (`character(1)`)
        .id = NULL,

        #' @field .data (`arrow::Table`)
        .data = NULL,

        #' @field .pull (`function`)
        .pull = NULL,

        #' @field .tidy (`function`)
        .tidy = NULL,

        #' @field .finished (`logical(1)`)\cr
        #'  `TRUE` if `$tidy()` has been called
        #'  successfully, otherwise `FALSE`.
        .finished = FALSE,

        .encode_func = function(func) {
            serialize(func, NULL) |>
                memCompress(type = "gzip") |>
                jsonlite::base64_enc()
        },

        .decode_func = function(func) {
            jsonlite::base64_dec(func) |>
                memDecompress(type = "gzip") |>
                unserialize()
        },

        .write_metadata = function() {
            private$.data$metadata <- list(
                id       = private$.id,
                finished = private$.finished,
                pull     = private$.encode_func(private$.pull),
                tidy     = private$.encode_func(private$.tidy)
            )
        },

        .read_metadata = function() {
            private$.id <- private$.data$metadata$id
            private$.finished <- as.logical(private$.data$metadata$finished)

            private$.pull <- private$.decode_func(private$.data$metadata$pull)
            private$.tidy <- private$.decode_func(private$.data$metadata$tidy)

            private$.data$metadata$finished <- NULL
            private$.data$metadata$pull     <- NULL
            private$.data$metadata$tidy     <- NULL
        }
    )
)
