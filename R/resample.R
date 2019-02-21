#' @title Resampling data
#'
#' @description Aggregate data by different time periods. Following this simple steps:
#' * split data in series
#'   - pad data serie (needed for calculation of capture threshold)
#'   - group serie by new interval with [lubridate::floor_date()]
#'   - apply statistical method or user provides function (user can provide list per parameter)
#' * combine resampled series
#'
#' @section TODO Wind:
#' Code optimieren ?
#' sicherstellen, dass alle Voraussetzungen erfüllt sind (stopifnot() etc) und korrektes bind_rows mit Faktoren
#' (Deine bind_rows_with... spuckt die Zahl aus, nicht einen Faktor, in meinem Fall)
#'
#' @param data A tibble in rOstluft long format
#' @param new_interval New interval. Must be longer than actual interval (not checked)
#' @param statistic Statistical method to apply when aggregating the data; default is the mean.
#'   Can be one of “mean”, “max”, “min”, “median”, “frequency”, “sd”, “percentile”. Note that “sd”
#'   is the standard deviation, “frequency” is the number (frequency) of valid records in the period
#'   and “data.cap” is the percentage data capture. “percentile” is the percentile level (\%) between
#'   0-100, which can be set using the “percentile” argument.
#'   Or a function with one argument expecting a vector.
#'   Or a list with parameter as name and the statistical method as value (function or name of method)
#' @param data.thresh minimum data capture threshold in \% to use
#' @param start.date optional start date for padding. Default min date in series floored to the new interval
#' @param end.date optional end date for padding. Default max date in series ceiled to the new interval
#' @param drop.last optional drop the last added time point by padding. Default False, true if no end.date
#'   provided and max date != ceiled max date.
#' @param percentile The percentile level in \% used when statistic = "percentile". The default is 95.
#'
#' @return tibble with resampled data
#' @export
resample <- function(data, statistic = "mean", new_interval, data.thresh = NULL,
                     start.date = NULL, end.date = NULL, drop.last = FALSE, rename.parameter = FALSE,
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

  # separate wind data from rest, if (per default) vector averages are required and treat wind data differently (! assumption: WD always in °)
  vector.avg <- stringr::str_detect(statistic, "vector.avg")
  wind.data <- NULL
  if (any(vector.avg)) {
    wind_parameters <- statistic[vector.avg]
    data <- cut_wind_data(data, names(wind_parameters))

    if (!is.null(data$wind)) {
      wind.data <- resample_wind(data$wind, wind_parameters, new_interval = new_interval, data.thresh = data.thresh,
                                 start.date = start.date, end.date = end.date, drop.last = drop.last)
    } else {
      wind.data <- NULL
    }

    data <- data$others
  }

  # treat skalar data as intended
  if (!is.null(data)) {
    data <- dplyr::group_by(data, .data$site, .data$parameter, .data$interval, .data$unit)
    group_keys <- dplyr::group_keys(data)
    statistic_list <- purrr::map(group_keys$parameter, get_statistic_for_serie)
    data <- dplyr::group_split(data)
    data <-  purrr::map2(data, statistic_list, resample_series, new_interval = new_interval, at = "value",
                         data.thresh = data.thresh, start.date = start.date, end.date = end.date, drop.last = drop.last,
                         rename.parameter = rename.parameter, percentile = percentile)
  }

  # TODO find a more elegant way?
  if (!is.null(wind.data) && !is.null(data)) {
    data <- rlang::exec(bind_rows_with_factor_columns, !!!data, wind.data)
  } else if (!is.null(data)) {
    data <- rlang::exec(bind_rows_with_factor_columns, !!!data)
  } else if (!is.null(wind.data)) {
    data <- rlang::exec(bind_rows_with_factor_columns, wind.data)
  } else {
    data <- NULL
  }
  data
}


#' Cut wind data
#'
#' This function splits
#'
#' @param data tibble with input data
#' @param wind_parameters Character Vector of Wind parameters. Default wind_parameters = c("WD", "WVv")
#'
#' @return named list: $wind all wind parameters, $others everything else

#' @keywords internal
cut_wind_data <- function(data, wind_parameters = c("WD", "WVv")) {
  data <- dplyr::group_by(data, isWind= .data$parameter %in% wind_parameters)
  keys <- dplyr::group_keys(data)
  data <- dplyr::group_split(data, keep = FALSE)
  mapping <- list("TRUE" = "wind", "FALSE" = "others")
  rlang::set_names(data, mapping[as.character(keys$isWind)])  # FALSE before TRUE, order don't matters
}


#TODO find the right place for this definition: statistic.R ?
rename_list <- list(
  "n" = "_nb_",
  "max" = "_max_",
  "min" = "_min_",
  "coverage" = "_coverage_"
)

update_unit <- list(
  "n" = "1",
  "data.cap" = "%"
)


#' @title resampling a serie
#' @param serie a tibble in rOstluft long format containing exactly one serie
#'
#' @return tibble with resampled series
#'
#' @rdname resample
#' @keywords internal
resample_series <- function(serie, statistic = "mean", new_interval = "d1", at = "value", data.thresh = NULL,
                            start.date = NULL, end.date = NULL, drop.last = FALSE, rename.parameter = FALSE,
                            percentile = 95) {

  interval.converted <- convert_interval(new_interval)
  old_interval <- dplyr::first(serie$interval)


  if (is.null(start.date)) {
    start.date <- lubridate::floor_date(min(serie$starttime), interval.converted)
  }

  if (is.null(end.date)) {
    end.max <- max(serie$starttime)
    end.date <- lubridate::ceiling_date(end.max, interval.converted)
    drop.last <- (end.max != end.date)  #TODO Decide if we should always drop last in this case
  }

  # TODO optional parameter to skip this
  serie <- pad_serie(serie, start.date, end.date, drop.last)


  serie <- dplyr::group_by(serie,
                           starttime = lubridate::floor_date(.data$starttime, interval.converted),
                           .data$site, .data$parameter, .data$interval, .data$unit
  )

  FUN <- statistic_fun_factory(statistic, data.thresh, percentile)
  serie <- dplyr::summarise(serie, value = FUN(!!! syms(at)))
  serie <- dplyr::ungroup(serie) # needed if we called resample_serie with grouped series

  #TODO could use forcats::fct_recode for Performance?
  serie <- dplyr::mutate(serie, interval = forcats::as_factor(new_interval))

  if (isTRUE(rename.parameter) && rlang::is_character(statistic) && statistic %in% names(rename_list)) {
    old_parameter <- as.character(dplyr::first(serie$parameter))
    parameter <- stringr::str_c(old_parameter, rename_list[[statistic]], old_interval)
    serie$parameter <- forcats::as_factor(parameter)
  }

  if (rlang::is_character(statistic) && statistic %in% names(update_unit)){
    unit <- update_unit[[statistic]]
    serie$unit <- forcats::as_factor(unit)
  }

  serie
}


#' Resamples wind data
#'
#' @return tibble with resampled wind data
#'
#' @rdname resample
#' @keywords internal
resample_wind <- function(data, statistic, new_interval = "d1", data.thresh = NULL, start.date = NULL, end.date = NULL,
                          drop.last = FALSE) {

  data <- dplyr::group_split(data, .data$site, .data$interval)
  data <- purrr::map(data, resample_wind_site, statistic = statistic, new_interval = new_interval,
                     data.thresh = data.thresh, start.date = start.date, end.date = end.date, drop.last = drop.last)

  rlang::exec(bind_rows_with_factor_columns, !!!data)
}


#' resamples wind
#'
#' @param data.site wind data for one specific site. should only contain wind speed and direction
#'
#' @return tibble with resampled wind speed and direction
#' @keywords internal
resample_wind_site <- function(data.site, statistic, new_interval = "d1", data.thresh = NULL,
                               start.date = NULL, end.date = NULL, drop.last = FALSE) {

  interval.converted <- convert_interval(new_interval)
  old_interval <- dplyr::first(data.site$interval)

  if (is.null(start.date)) {
    start.date <- lubridate::floor_date(min(data.site$starttime), interval.converted)
  }

  if (is.null(end.date)) {
    end.max <- max(data.site$starttime)
    end.date <- lubridate::ceiling_date(end.max, interval.converted)
    drop.last <- (end.max != end.date)  #TODO Decide if we should always drop last in this case
  }

  groups <- dplyr::group_by(data.site, .data$parameter, .data$unit)
  keys <- dplyr::group_keys(groups)
  site <- dplyr::first(data.site$site)
  groups <- dplyr::group_split(groups)

  ## TODO warning if not wd and ws in data
  if (length(groups) != 2) {
    warning(sprintf("resample_wind_site: expected 2 groups (wd and ws) for site %s", site))
    return(groups[[1]][0, ]) # not sure if groups[[1]] is a smart way
  }

  groups <- purrr::map(groups, pad_serie, start.date, end.date, drop.last)
  groups <- rlang::set_names(groups, statistic[as.character(keys$parameter)])

  ws <- groups$vector.avg.ws
  wd <- groups$vector.avg.wd

  # TODO test unit wd == ° => abort or use radian ?

  wind_components <- tibble::tibble(
    starttime = lubridate::floor_date(ws$starttime, interval.converted),
    U = ws$value * sin(2 * pi * wd$value / 360),
    V = ws$value * cos(2 * pi * wd$value / 360)
  )

  wind_components <- dplyr::group_by(wind_components, .data$starttime)
  FUN <- statistic_fun_factory("mean", data.thresh)
  wind_components <- dplyr::summarise(wind_components, U = FUN(.data$U), V = FUN(.data$V))
  wind_components <- dplyr::mutate(wind_components,
                                   wd = (atan2(.data$U, .data$V) * 360 / 2 / pi) %% 360,
                                   ws = sqrt(.data$U^2 + .data$V^2))

  ws <- tibble::tibble(
    starttime = wind_components$starttime,
    site = dplyr::first(ws$site),
    parameter = dplyr::first(ws$parameter),
    interval = forcats::as_factor(new_interval),
    unit = dplyr::first(ws$unit),
    value = wind_components$ws
  )

  wd <- tibble::tibble(
    starttime = wind_components$starttime,
    site = dplyr::first(wd$site),
    parameter = dplyr::first(wd$parameter),
    interval = forcats::as_factor(new_interval),
    unit = dplyr::first(wd$unit),
    value = wind_components$wd
  )

  bind_rows_with_factor_columns(wd, ws)
}

#' Pad data
#'
#' Function to pad out missing time points
#'
#' @param data tibble in rOstluft long format
#' @param start.date optional start date for padding. Default min date in series
#' @param end.date optional end date for padding. Default max date in series
#' @param drop.last optional drop the last added time point by padding. Useful when
#'   resampling and end.date is the first time point of the new interval
#'
#' @return tibble with padded data
#'
#' @export
pad <- function(data, start.date = NULL, end.date = NULL, drop.last = FALSE) {
  data.grouped <- dplyr::group_by(data, .data$site, .data$parameter, .data$interval, .data$unit)
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
    start.date <- min(serie$starttime)
  }

  if (is.null(end.date)) {
    end.date <- max(serie$starttime)
    drop.last <- FALSE
  }

  # by joining the data we insert rows with NA values for site, parameter, interval, unit, value
  # we need to fill this with the values from the supplied df
  fill.values <- dplyr::slice(serie, 1)
  fill.values <- as.list(dplyr::select(fill.values, -.data$starttime, -.data$value))

  interval <- convert_interval(fill.values$interval)

  all.dates <- tibble::tibble(
    starttime = seq(start.date, end.date, interval)
  )

  if (isTRUE(drop.last)) {
    all.dates <- utils::head(all.dates, -1)
  }

  padded <- dplyr::full_join(all.dates, serie, by = "starttime")
  tidyr::replace_na(padded, replace = fill.values)
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
