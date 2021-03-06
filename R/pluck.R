#' Quickly filter data
#'
#' All pluck funtions are wrappers around `dplyr::filter()`. The dots arguments are converted in a vector and then each
#' function searchs the specific column with the `%in%` operator.
#'
#' @section Windows, symbols and encodings:
#' The first implementation supported symbols as dots arguments. On windows the symbol gets converted to the native
#' encoding without the encoding mark. Is the converted symbol now compared to UTF-8 strings it doesn't match. One
#' example would be strings like "°C" or "µg/m3". See `rlang::as_string()`
#'
#'
#' @param data input data in rolf format
#' @param ... elements to get as strings or integers
#'
#' @rdname pluck
#'
#' @return filtered input data
#' @export
#'
#' @examples
#' fn <- system.file("extdata", "smn_multi.txt", package = "rOstluft.data")
#' data <- read_smn_multiple(fn) %>% dplyr::arrange(starttime)
#'
#' pluck_parameter(data, "ta1towb0")
#'
#' # strings or symbols
#' pluck_site(data, "KLO", "UEB")
#'
#' # supports splicing with !!!
#' intervals = c("h1", "d1")
#' pluck_interval(data, !!!intervals) %>% dplyr::slice(40:45)
#'
#' # pluck_year supports vector
#' pluck_year(data, 2010:2012)
#'
#' # NAs in data aren't a problem
#' pluck_unit(data, "hPa")
#'
#' # pipe friendly
#' data %>%
#'   pluck_site("KLO", "UEB") %>%
#'   pluck_parameter("gre000z0") %>%
#'   pluck_year(2010:2018)
pluck_parameter <- function(data, ...) {
  search <- purrr::flatten(rlang::list2(...))
  dplyr::filter(data, .data$parameter %in% search)
}

#' @rdname pluck
#' @export
pluck_interval <- function(data, ...) {
  search <- purrr::flatten(rlang::list2(...))
  dplyr::filter(data, .data$interval %in% search)
}

#' @rdname pluck
#' @export
pluck_site <- function(data, ...) {
  search <- purrr::flatten(rlang::list2(...))
  dplyr::filter(data, .data$site %in% search)
}

#' @rdname pluck
#' @export
pluck_unit <- function(data, ...) {
  search <- purrr::flatten(rlang::list2(...))
  dplyr::filter(data, .data$unit %in% search)
}

#' @rdname pluck
#' @export
pluck_year <- function(data, ...) {
  # Only strings can be converted to symbols
  search <- purrr::flatten(rlang::list2(...))
  dplyr::filter(data, lubridate::year(.data$starttime) %in% search)
}
