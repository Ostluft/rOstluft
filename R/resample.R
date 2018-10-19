#' @title Resampling data
#'
#' @description Aggregate data by different time periods. Following this simple steps:
#' * split data in series
#'   - pad data serie (needed for calculation of capture threshold)
#'   - group serie by new intervall with [lubridate::floor_date()]
#'   - apply statistical method or user provides function (user can provide list per parameter)
#' * combine resampled series
#'
#' @section TODO Wind :
#' Need a solution for wind vectoring ... this isn't as simple. probably need group all wind and non
#' wind parameters. Handle wind like now and do something special with the wind vars. One possible to
#' group:
#' ```
#' dg <- dplyr::group_by(data, isWind = .data$parameter %in% c("WD", "WVv", "WVs"))
#' tidyr::nest()
#' A tibble: 2 x 2
#'  isWind data
#'  <lgl>  <list>
#'  1 FALSE  <tibble [208,344 x 6]>
#'  2 TRUE   <tibble [35,037 x 6]>
#' ```
#' This was the easy part :P. now join the wind data, calc vector mean and return.
#'
#' @param data A tibble in rOstluft long format
#' @param new_intervall New Intervall. Must be longer than actual Intervall (not checked)
#' @param statistic Statistical method to apply when aggregating the data; default is the mean.
#'   Can be one of “mean”, “max”, “min”, “median”, “frequency”, “sd”, “percentile”. Note that “sd”
#'   is the standard deviation, “frequency” is the number (frequency) of valid records in the period
#'   and “data.cap” is the percentage data capture. “percentile” is the percentile level (\%) between
#'   0-100, which can be set using the “percentile” argument.
#'   Or a function with one argument expecting a vector.
#'   Or a list with parameter as name and the statistical method as value (function or name of method)
#' @param data.thresh minimum data capture threshold in \% to use
#' @param start.date optional start date for padding. Default min date in series floored to the new intervall
#' @param end.date optional end date for padding. Default max date in series ceiled to the new intervall
#' @param drop.last optional drop the last added time point by padding. Default False, true if no end.date
#'   provided and max date != ceiled max date.
#' @param percentile The percentile level in \% used when statistic = "percentile". The default is 95.
#'
#' @return tibble with resampled data
#' @export
resample <- function(data, new_intervall, statistic = "mean", data.thresh = NULL,
                     start.date = NULL, end.date = NULL, drop.last = FALSE,
                     percentile = 95) {

  # build helper function to get the statistical method for the parameter
  if (!is.list(statistic)) {
    get_statistic_for_serie <- function(parameter) {
      statistic
    }
  } else {
    default_statistic <- getElement2(statistic, "default_statistic", "mean")
    get_statistic_for_serie <- function(parameter) {
      getElement2(statistic, as.character(parameter), default_statistic)
    }
  }

  data.grouped <- dplyr::group_by(data, .data$station, .data$parameter, .data$intervall, .data$einheit)
  data.grouped <- dplyr::do(data.grouped, resample_series(
    .data, new_intervall, get_statistic_for_serie(dplyr::first(.data$parameter)),
    start.date, end.date, data.thresh, drop.last, percentile
  ))
  dplyr::ungroup(data.grouped)
}


# resample_wind <- function(data, new_intervall, ...) {
#
#   data.grouped <- dplyr::group_by(dfs, isWind = .data$parameter %in% c("WD", "WVv", "WVs"))
#   data.grouped <- tidyr::nest(data.grouped)
#
#   data.grouped <- tibble::deframe(data.grouped)
#
#   wind_vectoring(data.grouped[["TRUE"]], new_intervall)
#
#
# }
#
# wind_vectoring <- function(data) {
#   data
#}


#' @title resampling a serie
#' @param serie a tibble in rOstluft long format containing exactly one serie
#'
#' @rdname resample
#' @export
resample_series <- function(serie, new_intervall, statistic = "mean", data.thresh = NULL,
                            start.date = NULL, end.date = NULL, drop.last = FALSE,
                            percentile = 95) {

  intervall.converted <- convert_intervall(new_intervall)


  if (is.null(start.date)) {
    start.date <- lubridate::floor_date(min(serie$startzeit), intervall.converted)
  }

  if (is.null(end.date)) {
    end.max <- max(serie$startzeit)
    end.date <- lubridate::ceiling_date(end.max, intervall.converted)
    drop.last <- (end.max != end.date)  #TODO Decide if we should always drop last in this case
  }

  # TODO optional parameter to skip this
  serie <- pad_serie(serie, start.date, end.date, drop.last)


  serie <- dplyr::group_by(serie,
                            startzeit = lubridate::floor_date(.data$startzeit, intervall.converted),
                            .data$station, .data$parameter, .data$intervall, .data$einheit
  )
  FUN <- statistic_fun_factory(statistic, data.thresh, percentile)
  serie <- dplyr::summarise_at(serie, "wert", FUN)
  serie <- dplyr::ungroup(serie) # needed if we called resample_serie with grouped series
  dplyr::mutate(serie, intervall = new_intervall)
}


#' Pad data
#'
#' Function to pad out missing time points
#'
#' @param data tibble in rOstluft long format
#' @param start.date optional start date for padding. Default min date in series
#' @param end.date optional end date for padding. Default max date in series
#' @param drop.last optional drop the last added time point by padding. Useful when
#'   resampling and end.date is the first time point of the new interval#'
#' @return tibble with padded data
#'
#' @export
pad <- function(data, start.date = NULL, end.date = NULL, drop.last = FALSE) {
  data.grouped <- dplyr::group_by(data, .data$station, .data$parameter, .data$intervall, .data$einheit)
  data.grouped <- dplyr::do(data.grouped, pad_serie(.data, start.date, end.date, drop.last))
  dplyr::ungroup(data.grouped)
}

#' Pad serie
#'
#' @param serie a tibble in rOstluft long format containing exactly one serie
#'
#' @rdname pad
#' @export
pad_serie <- function(serie, start.date = NULL, end.date = NULL, drop.last = FALSE) {
  if (is.null(start.date)) {
    start.date <- min(serie$startzeit)
  }

  if (is.null(end.date)) {
    end.date <- max(serie$startzeit)
    drop.last <- FALSE
  }

  # by joining the data we insert rows with NA values for station, parameter, intervall, einheit, wert
  # we need to fill this with the values from the supplied df
  fill.values <- dplyr::slice(serie, 1)
  fill.values <- as.list(dplyr::select(fill.values, -.data$startzeit, -.data$wert))

  intervall <- convert_intervall(fill.values$intervall)

  all.dates <- tibble::tibble(
    startzeit = seq(start.date, end.date, intervall)
  )

  if (isTRUE(drop.last)) {
    all.dates <- utils::head(all.dates, -1)
  }

  padded <- dplyr::full_join(all.dates, serie, by = "startzeit")
  tidyr::replace_na(padded, replace = fill.values)
}



convert_intervall <- function(intervall) {
  num <- stringr::str_extract(intervall, "[:digit:]+")
  units <- stringr::str_extract(intervall, "[:alpha:]+")
  units <- stringr::str_to_lower(units)
  if (is.na(num)) num <- "1"
  if (units == "m") units <- "month"
  if (units == "y") units <- "year"

  stringr::str_c(num, units, sep = " ")
}
