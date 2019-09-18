#' @title rOstluft long format (rolf)
#'
#' @description Data format for storing air quality data. A Serie is the combination of measurement site, interval,
#' parameter and unit.
#'
#' The data is chunked in year of measurement, site and interval. Base64 encoding is used to avoid invalid characters
#'
#' @field index index for the data (in most cases the time columns)
#' @field serie_columns combination of columns for a serie
#' @field chunk_columns data columns for chunking
#' @field chunk_calc formulas for calculating new columns for chunking
#' @field unique_columns = index + serie_columns
#' @field content_columns = serie_columns + names(chunk_calc)
#' @field tz time zone used for chunking. Same time different chunk:
#'  2018-01-01T00:00:00+0100 = 2018, 2017-12-31T23:00:00+0000 = 2017
#'
#' @section Methods:
#'
#' `$sort(data)`
#'
#' `$merge(new_data, old_data)`
#'
#' `$chunk_name(chunk_data)` returns the chunkname based on the first row of the supplied data
#'
#' `$get_chunk_names(interval, site, year)` returns encoded chunknames for all combinations of the supplied vectors
#'
#' `$encode_chunk_name(interval, site, year)` returns encoded chunkname
#'
#' `$decode_chunk_name(chunk_name)` returns decoded chunk_name in tibble as columns (chunk_name, interval, site, year)
#'
#' `$destroy(confirmation)` removes all files under path from the file system if "DELETE" is supplied as
#' confirmation
#'
#' @section TODO:
#'
#' Update Documentation is outdated
#'
#' @name format_rolf
#' @docType class
NULL

#' @param tz time zone used for chunking.  Default Etc/GMT-1
#'
#' @return R6 class object of format_rolf
#' @export
format_rolf <- function(tz = "Etc/GMT-1") {
  r6_format_rolf$new(tz = tz)
}


r6_format_rolf <- R6::R6Class(
  "format_rolf",
  public = list(
    index = "starttime",
    value_column = "value",
    serie_columns = c("interval", "site", "parameter", "unit"),
    chunk_columns = c("interval", "site"),
    chunk_calc = NULL,
    unique_columns = NULL,
    # content columns should probably contain chunk_calc names,
    content_columns = c("year", "interval", "site", "parameter", "unit"),
    tz = NULL,
    initialize = function(tz = "Etc/GMT-1") {
      self$tz <- tz
      self$chunk_calc <- rlang::quos(year = lubridate::year(lubridate::with_tz(starttime, tz)))
      self$unique_columns <- c(self$index, self$serie_columns)
    },
    sort = function(data, na.rm = TRUE) {
      if (isTRUE(na.rm)) {
        data <- self$na.omit(data)
      }
      dplyr::arrange(data, .data$starttime)
    },
    merge = function(new_data, old_data) {
      format_merge(new_data, old_data, self$unique_columns)
    },
    na.omit = function(data) {
      dplyr::filter(data, !is.na(.data$value))
    },
    chunk_name = function(chunk_data) {
      # better way to do it?
      row <- dplyr::slice(chunk_data, 1)
      row <- dplyr::mutate(row, !!!self$chunk_calc)
      self$encode_chunk_name(row$interval, row$site, row$year)
    },
    chunk_vars = function(chunk_data) {
      # better way to do it?
      row <- dplyr::slice(chunk_data, 1)
      row <- dplyr::mutate(row, !!!self$chunk_calc)
      dplyr::select(row, dplyr::one_of(c(names(self$chunk_calc), self$chunk_columns)))
    },
    encode_chunk_name = function(interval, site, year) {
      fn <- base64url::base64_urlencode(stringr::str_c(interval, site, year, sep = "\u00bb"))
      fs::path_join(c(as.character(interval), fn))
    },
    encoded_chunk_names = function(interval, site, year) {
      chunk_names <- tidyr::expand(tibble::tibble(), interval, site, year)
      dplyr::transmute(chunk_names, chunk_name = purrr::pmap(chunk_names, self$encode_chunk_name))
    },
    decode_chunk_name = function(chunk_name) {
      if (is.na(chunk_name)) {
        tibble::tibble(
          chunk_name = character(),
          interval = character(),
          site = character(),
          year = character()
        )
      } else {
        fn <- tibble::tibble(chunk_name = chunk_name)
        fn <- dplyr::mutate(fn, chunk_part =  fs::path_file(.data$chunk_name))
        fn <- dplyr::mutate(fn, chunk_part =  base64url::base64_urldecode(.data$chunk_part))
        tidyr::separate(fn, "chunk_part", c("interval", "site", "year"), sep = "\u00bb")
      }
    }
  )
)

format_merge <- function(new_data, old_data, unique_columns) {
  xy <- bind_rows_with_factor_columns(new_data, old_data)
  dplyr::distinct(xy, !!!rlang::syms(unique_columns), .keep_all = TRUE)
}
