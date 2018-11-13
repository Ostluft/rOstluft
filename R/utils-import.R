
#' Imports directory into store
#'
#' @param store instance of the store to import data into
#' @param path to directory containing the data files
#' @param read_function function with argument file, ... is passed to this function to supply additionals arguments
#' @param glob see [fs::dir_ls()]
#' @param regexp see [fs::dir_ls()]
#' @param recursive see [fs::dir_ls()]
#' @param ... passed to the read_function
#'
#' @return NULL
#' @export
#'
#' @examples
#' format <- rOstluft::format_rolf()
#' store <- rOstluft::storage_local_rds("example_rOstluft", format, read.only = FALSE)
#' path <- system.file("extdata", package = "rOstluft.data")
#'
#' rOstluft::import_directory(store, path, rOstluft::read_airmo_csv)
#'
#' store$get_content()
#'
#' store$destroy("DELETE")
#'
import_directory <- function(store, path, read_function, glob = NULL, regexp = NULL, recursive = TRUE, ...) {
  files <- fs::dir_ls(path, glob = glob, regexp = regexp, recursive = recursive, type = "file")
  i <- 1
  l <- length(files)
  op <- list(digits.secs = 4, dplyr.show_progress = FALSE)
  op_prev <- purrr::map(names(op), getOption)
  names(op_prev) <- names(op)
  options(op)

  t_import_start <- Sys.time()

  for (file in files) {
    fn_info <- fs::file_info(file)
    message(sprintf("Importing '%s' with size %s. File %d of %d", file, fn_info$size, i, l))
    t_read_start <- Sys.time()
    data <- read_function(file, ...)
    t_read_end <- Sys.time()
    t_read <- lubridate::time_length(t_read_end - t_read_start, unit = "seconds")
    message(sprintf("Read '%s' in %.2f seconds. Got %d data points",
            file, t_read, nrow(data)))

    store$put(data)
    t_put_end <- Sys.time()
    t_put <- lubridate::time_length(t_put_end - t_read_end, unit = "seconds")
    message(sprintf("Put data into store %s in %.2f seconds", store$name, t_put))
    i <- i + 1
  }

  t_import_end <- Sys.time()
  t_import <- lubridate::time_length(t_import_end - t_import_start, unit = "seconds")
  message(sprintf("Finished import after %.2f seconds", t_import))

  options(op_prev)
  invisible(NULL)
}
