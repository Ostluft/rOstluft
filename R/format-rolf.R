#' @title rOstluft long format (rolf)
#'
#' @description Data format for storing air quality data. A Serie is the combination of measurement site, interval, parameter
#' and unit.
#'
#' The data is chunked in year of measurement, site and interval. Base64 encoding is used to avoid invalid characters
#'
#' @field index index for the data (in most cases the time columns)
#' @field serie_columns combination of columns for a serie
#' @field chunk_columns data columns for chunking
#' @field chunk_calc formulas for calculating new columns for chunking
#' @field unique_columns = index + serie_columns
#' @field content_columns = serie_columns + names(chunk_calc)
#'
#' @section Methods:
#'
#' `$sort(data)`
#'
#' `$merge(old_data, new_data)`
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
#' @name format_rolf
#' @docType class
NULL

#' @return R6 class object of format_rolf
#' @export
format_rolf <- function() {
  r6_format_rolf$new()
}


r6_format_rolf <- R6::R6Class(
  'format_rolf',
  public = list(
    index = "starttime",
    value_column = "value",
    serie_columns = c("interval", "site", "parameter", "unit"),
    chunk_columns = c("interval", "site"),
    chunk_calc = list("year" = ~lubridate::year(starttime)),
    unique_columns = NULL,
    content_columns = NULL,
    initialize = function() {
      self$unique_columns <- c(self$index, self$serie_columns)
      self$content_columns <- c(self$serie_columns, names(self$chunk_calc))
    },
    sort = function(data) {
      dplyr::arrange(data, .data$starttime)
    },
    merge = function(old_data, new_data) {
      format_merge(old_data, new_data, self$unique_columns)
    },
    chunk_name = function(chunk_data) {
      # better way to do it?
      row <- dplyr::slice(chunk_data, 1)
      self$encode_chunk_name(row$interval, row$site, lubridate::year(row$starttime))
    },
    get_chunk_names = function(interval, site, year) {
      chunk_names <- tidyr::expand(tibble::tibble(), interval, site, year)
      dplyr::transmute(chunk_names, chunk_name = purrr::pmap(chunk_names, self$encode_chunk_name))
    },
    encode_chunk_name = function(interval, site, year) {
      fn <- base64url::base64_urlencode(paste(paste(interval, site, year, sep = "»"), sep = "»"))
      fs::path_join(c(as.character(interval), fn))
    },
    decode_chunk_name = function(chunk_name) {
      fn <- tibble::tibble(chunk_name = chunk_name)
      fn <- dplyr::mutate(fn, chunk_part =  fs::path_file(.data$chunk_name))
      fn <- dplyr::mutate(fn, chunk_part =  base64url::base64_urldecode(.data$chunk_part))
      tidyr::separate(fn, "chunk_part", c("interval", "site", "year"), sep = "»")
    }
  )
)

format_merge <- function(old_data, new_data, unique_columns) {
  xy <- bind_rows_with_factor_columns(new_data, old_data)
  dplyr::distinct(xy, !!!rlang::syms(unique_columns), .keep_all = TRUE)
}
