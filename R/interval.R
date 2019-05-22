#' Detects the interval of the data
#'
#' Throws error if multiple Intervals are detected
#'
#' @param data input data in rolf format
#'
#' @return interval as character string
#'
#' @keywords internal
detect_interval <- function(data) {
  interval <- dplyr::distinct(data, .data$interval)

  if (nrow(interval) != 1) {
    stop("multiple intervals in data")
  }

  as.character(interval[[1]])
}


#' Converts a AIRMO Zeitfenster to an interval
#'
#' This function is far from perfect but works with the non offseting, shifting Zeitfenster
#'
#' @param interval string with AIRMO Zeitfenster
#'
#' @return string with valid string for [base::seq.Date()] or [lubridate::floor_date()]
#'
#' @keywords internal
convert_interval <- function(interval) {
  num <- stringr::str_extract(interval, "[:digit:]+")
  units <- stringr::str_extract(interval, "[:alpha:]+")
  units <- stringr::str_to_lower(units)
  if (is.na(num)) num <- "1"
  if (units == "m") units <- "month"
  if (units == "y") units <- "year"

  stringr::str_c(num, units, sep = " ")
}
