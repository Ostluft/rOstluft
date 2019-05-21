#' @title Format defintion for ps data with endtime instead of interval
#'
#' @description
#' This format is used for non periodical data
#'
#' @section TODO:
#' Define class of columns
#'
#' @section Columns:
#'
#' * **starttime**: POSIXct
#' * **endtime**: POSIXct
#' * **site**: factor/character
#' * **parameter**: factor/character
#' * **unit**: factor/character
#' * **value**: double
#'
#' @section Series:
#'
#' A serie is the unique combination of the columns:
#'
#' * site
#' * parameter
#' * unit
#'
#' @section Chunking:
#'
#' The data is chunked by the columns
#'
#' * site
#'
#' @section Content Columns:
#'
#' * year = year(with_tz(starttime, tz))
#' * site
#' * parameter
#' * unit
#' * n = number of valid values
#'
#' @name format_ps
#' @docType class
NULL


#' @param tz time zone used for chunking/content. Default Etc/GMT-1
#'
#' @return R6 class object of format_rolf
#' @export
format_ps <- function(tz = "Etc/GMT-1") {
  r6_format_ps$new(tz = tz)
}


r6_format_ps <- R6::R6Class(
  "format_ps",
  public = list(
    index = "starttime",
    value_column = "value",
    serie_columns = c("site", "parameter", "unit"),
    chunk_columns = c("site"),
    chunk_calc = character(),
    unique_columns = c("starttime", "endtime", "site", "parameter", "unit"),
    # content columns should probably contain chunk_calc names,
    content_columns = NULL,
    tz = NULL,
    initialize = function(tz = "Etc/GMT-1") {
      self$tz <- tz
      self$content_columns <- c(
        "year" = rlang::parse_expr(sprintf("lubridate::year(lubridate::with_tz(starttime, '%s'))", tz)),
        "site", "parameter", "unit"
      )
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
      self$encode_chunk_name(row$site)
    },
    chunk_vars = function(chunk_data) {
      # better way to do it?
      row <- dplyr::slice(chunk_data, 1)
      row <- dplyr::mutate(row, !!!self$chunk_calc)
      dplyr::select(row, dplyr::one_of(c(names(self$chunk_calc), self$chunk_columns)))
    },
    encode_chunk_name = function(site) {
      base64url::base64_urlencode(site)
    },
    encoded_chunk_names = function(site) {
      chunk_names <- tidyr::expand(tibble::tibble(), site)
      dplyr::transmute(chunk_names, chunk_name = purrr::pmap(chunk_names, self$encode_chunk_name))
    },
    decode_chunk_name = function(chunk_name) {
      if (is.na(chunk_name)) {
        tibble::tibble(
          chunk_name = character(),
          site = character()
        )
      } else {
        fn <- tibble::tibble(chunk_name = chunk_name)
        fn <- dplyr::mutate(fn, chunk_part =  fs::path_file(.data$chunk_name))
        fn <- dplyr::mutate(fn, chunk_part =  base64url::base64_urldecode(.data$chunk_part))
        dplyr::select(fn, chunk_name, site = chunk_part)
      }
    }
  )
)
