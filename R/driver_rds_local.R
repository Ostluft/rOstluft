#' @title Local rds store
#'
#' @field name name of the store
#' @field path root of the store
#' @field rds_path root of all chunks
#' @field rds_content path to the rds file containing statistics of store content
#' @field read.only flag for read.only usage of store
#'
#' @section Methods:
#' `$type()` returns type of store as string
#'
#' `$get(station, intervall, year, parameters = NULL)` get data from the store. see examples for more information
#'
#' `$put(data)` puts the data into the store. Stops if store is read only
#'
#' `$get_content()` get n data points grouped by intervall, station, parameter, year in the store
#'
#' `$get_list_of_chunks()` get list of all chunks
#'
#' `$get_chunk_path(intervall, station, year)` get the path of chunk from grouping variables
#'
#' `$decode_chunk_filename(chunk_path)` decode base64 filename of chunk
#'
#' `$destroy(confirmation)` removes all files under path from the file system if "DELETE" is supplied as
#' confirmation
#'
#' @examples
#' ## init store, creates directory if necessary
#' store <- rOstluft::driver_rds_local("example_rOstluft", read.only = FALSE)
#'
#' ## read data from airmo export und put into the store
#' fn <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",
#'                   package = "rOstluft.data", mustWork = TRUE)
#' df <- rOstluft::read_airmo_csv(fn)
#' store$put(df)
#'
#' ## get all data min30 for 2011 and 2012
#' store$get("Zch_Stampfenbachstrasse", "min30", 2011:2012)
#'
#' ## get only data for O3
#' store$get("Zch_Stampfenbachstrasse", "min30", 2011:2012, "O3")
#'
#' ## get NOx data from multiple stations
#' store$get(c("Zch_Stampfenbachstrasse", "Zch_Rosengartenstrasse"), "min30",
#'           2014, c("NOx", "NO", "NO2"))
#'
#' ## get n data points grouped by intervall, station, parameter, year in the store
#' store$get_content()
#'
#' ## get list of all chunks
#' store$get_list_of_chunks()
#'
#' ## get the path of chunk from grouping variables
#' store$get_chunk_path("min30", "Zch_Stampfenbachstrasse", 2012)
#'
#' ## decode base64 filename of chunk
#' store$decode_chunk_filename("bWluMzAtWmNoX1N0YW1wZmVuYmFjaHN0cmFzc2UtMjAxMg.rds")
#'
#' ## destroy store (careful removes all files on the disk)
#' store$destroy("DELETE")
#'
#' @name driver_rds_local
#' @docType class
NULL

#' @param name name of the store
#' @param path optional path to create the store under.
#'   Defaults to [rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")][rappdirs::user_data_dir()]
#' @param read.only read only store. disable put, if false and the store doesn't exist, the store will be initiated
#'
#' @return R6 driver_rds_local class Object
#' @export
#'
driver_rds_local <- function(name, path = NULL, read.only = TRUE) {
  r6_driver_rds_local$new(name, path, read.only)
}


r6_driver_rds_local <- R6::R6Class(
  "driver_rds_local",
  public = list(
    name = NULL,
    path = NULL,
    rds_path = NULL,
    rds_content = NULL,
    read.only = TRUE,

    initialize = function(name, path = NULL, read.only = TRUE) {
      if (is.null(path)) {
        path <- rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")
      } else {
        path <- fs::path_abs(path)
      }

      self$name <- name
      self$path <- path
      self$rds_path <- fs::path(path, "rds")
      self$rds_content <- fs::path(self$path, "content.rds")
      self$read.only <- read.only

      is_new <- !fs::dir_exists(self$path)

      if (read.only && is_new) {
        stop(RdsLocalNotFound(name, self$path))
      }

      if (is_new) {
        fs::dir_create(file.path(self$path, "rds"), recursive = TRUE)
        fs::dir_create(file.path(self$path, "input"), recursive = TRUE)
        fs::dir_create(file.path(self$path, "tmp"), recursive = TRUE)
        fs::dir_create(file.path(self$path, "log_storr"), recursive = TRUE)

        content_table <- tibble::tibble(
          station = factor(),
          parameter = factor(),
          einheit = factor(),
          intervall = factor(),
          jahr = factor(),
          n = integer()
        )

        saveRDS(content_table, self$rds_content)

        message(sprintf("Local rds store %s initialized under '%s'", self$name, self$path))
        invisible(self)
      }
    },

    type = function() {
      "driver_rds_local"
    },

    put = function(data) {
      if (self$read.only) {
        stop(ReadOnlyStore(self$name))
      }
      data_grouped <- dplyr::group_by(data, jahr = as.factor(lubridate::year(.data$startzeit)),
                                      .data$intervall, .data$station)

      content_new <- dplyr::do(data_grouped, merge_chunk_into_rds_local_store(.data, self))
      content_new <- dplyr::ungroup(content_new)
      content_old <- readRDS(self$rds_content)
      saveRDS(merge_content(content_old, content_new), self$rds_content)
      content_new
    },

    get = function(station, intervall, year, parameters = NULL) {
      files <- tidyr::expand(data.frame(), station, intervall, year)
      files <- dplyr::transmute(files, chunk_path =
                                  self$get_chunk_path(.data$intervall, .data$station, .data$year))
      files <- dplyr::mutate(files, exists = fs::file_exists(.data$chunk_path))
      files <- dplyr::filter(files, .data$exists == TRUE)
      chunks <- purrr::map(files$chunk_path, read_chunk, parameters = parameters)
      chunks <- purrr::invoke(bind_rows_with_factor_columns, .x = chunks)
      droplevels(chunks)
    },

    get_chunk_path = function(intervall, station, year) {
      fn <- base64url::base64_urlencode(paste(intervall, station, year, sep = "-"))
      file.path(self$rds_path, intervall, paste0(fn, ".rds"))
    },

    decode_chunk_filename = function(chunk_path) {
      file_name <- fs::path_file(chunk_path)
      base64url::base64_urldecode(fs::path_ext_remove(file_name))
    },

    get_list_of_chunks = function() {
      chunks <- fs::dir_ls(self$rds_path, recursive = TRUE, type = "file")
      chunks <- tibble::as_tibble(list(file_name = chunks), rownames = NULL)
      chunks <- dplyr::mutate(chunks, chunk = decode_chunk_filename(.data$file_name))
      chunks <- dplyr::select(chunks, .data$chunk, .data$file_name)
      dplyr::arrange(chunks, .data$chunk)
    },

    get_content = function() {
      readRDS(self$rds_content)
    },

    destroy = function(confirmation = "NO") {
      if (confirmation == "DELETE" && self$read.only == FALSE) {
        fs::dir_delete(self$path)
        message(sprintf("Store %s destroyed", self$name))
      } else {
        warning("Store still alive: read.only store or wrong confirmation phrase")
      }
    }
  ),
  private = list(

  )
)


merge_chunk_into_rds_local_store <- function(chunk_data, store) {
  chunk_vars <- dplyr::slice(chunk_data, 1)
  chunk_path <- store$get_chunk_path(chunk_vars$intervall, chunk_vars$station, chunk_vars$jahr)
  dfn <- dplyr::select(chunk_data, -.data$jahr)  # drop year column introduced trough grouping

  if (fs::file_exists(chunk_path)) {
    dfo <- readRDS(chunk_path)
    dfn <- merge_rOstluft_longformat(dfo, dfn)
  } else {
    fs::dir_create(fs::path_dir(chunk_path))
    dfn <- dplyr::arrange(dfn, .data$startzeit)
  }
  dfn <- droplevels(dfn)
  saveRDS(dfn, chunk_path)
  res <- dplyr::count(dfn, .data$station, .data$parameter, .data$intervall, .data$einheit)
  res
}

read_chunk <- function(chunk_path, parameters) {
  data <- readRDS(chunk_path)
  if (!is.null(parameters)) {
    data <- dplyr::filter(data, .data$parameter %in% parameters)
  }
  data
}

decode_chunk_filename <- function(chunk_path) {
  file_name <- fs::path_file(chunk_path)
  base64url::base64_urldecode(fs::path_ext_remove(file_name))
}
