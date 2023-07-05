#' @export
tar_format_data_source <- targets::tar_format(
    read = function(path) {
        climateR.catalogs::data_source$new()$from_ipc_file(path)
    },
    write = function(object, path) {
        object$to_ipc_file(path)
    },
    marshal = function(object) {
        object$to_ipc_stream()
    },
    unmarshal = function(object) {
        climateR.catalogs::data_source$new()$from_ipc_stream(object)
    }
)


# -----------------------------------------------------------------------------

#' @export
tar_format_arrow_ipc <- targets::tar_format(
    read = function(path) {
        arrow::read_ipc_file(path, as_data_frame = FALSE)
    },
    write = function(object, path) {
        arrow::write_ipc_file(object, path)
        path
    },
    marshal = function(object) {
        arrow::write_to_raw(object, format = "stream")
    },
    unmarshal = function(object) {
        arrow::read_ipc_stream(object, as_data_frame = FALSE)
    }
)