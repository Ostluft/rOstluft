#' @title Resampling data
#'
#' @description
#' Aggregate data by different time periods. Following this simple steps:
#' * split data in series
#'   - pad data serie (needed for calculation of capture threshold, detection of gaps)
#'   - group serie by new interval with [lubridate::floor_date()]
#'   - apply statistical method or user provides function (user can provide list per parameter)
#' * combine resampled series
#'
#' It is possible to supply different methods for different parameters. The argument statistic can be named list. The
#' name stands for the parameter. The value can be a function to apply, a name of method or a list of names. Some
#' methods renames the parameter and changes the unit. A list of method names can only contain one non renaming method.
#'
#' @section Statistical methods:
#' The statistical method is a function with a numeric vector as argument and returns a single value.
#'
#' * `"mean"` average value
#' * `"median"` median value
#' * `"sd"` standard deviation of values
#' * `"sum"` sum over all values
#' * `"max"` maxium value
#' * `"min"` minimum value
#' * `"n"` number of valid records, renames parameter, changes unit
#' * `"coverage"` percentage of valid records, renames parameter, changes unit
#' * `"percentile"` calculates the percentile. Use the argument percentile to specify the level, renames parameter
#' * `"perc95"` 95% percentile, renames parameter
#' * `"perc98"` 98% percentile, renames parameter
#' * `"n>8"` number of values > 8 (CO d1 limit), renames parameter, changes unit
#' * `"n>50"` number of values > 50 (PM10 d1 limit), renames parameter, changes unit
#' * `"n>65"` number of values > 65 (O3 d1 indicator), renames parameter, changes unit
#' * `"n>80"` number of values > 80 (NO2 d1 limit), renames parameter, changes unit
#' * `"n>100"` number of values > 100 (SO2 d1 limit), renames parameter, changes unit
#' * `"n>120"` number of values > 120 (O3 h1 limit), renames parameter, changes unit
#' * `"n>160"` number of values > 160 (O3 h1 indicator), renames parameter, changes unit
#' * `"n>180"` number of values > 180 (O3 h1 indicator), renames parameter, changes unit
#' * `"n>200"` number of values > 200 (O3 h1 indicator), renames parameter, changes unit
#' * `"n>240"` number of values > 240 (O3 h1 indicator), renames parameter, changes unit
#' * `"drop"` drops the parameter from the result, useful for persons too lazy to filter the input data
#'
#' @section Wind:
#' Wind is a special case. For vector averaging the methods needs two inputs (direction and speed). To resample wind
#' data it is necessary to specify three parameters with the methods `"wind.direction"`, `"wind.speed_vector"` and
#' `"wind.speed_scalar"`. Even if scalar or vector speed isn't present. The parameter will be substituted by the other.
#'
#' Important: Wind calculation are standalone. It is possible to calculate multiple methods for non wind parameters.
#'
#' @section TODO:
#' * AOT40 statistic?
#' * some from https://github.com/davidcarslaw/openair/blob/master/R/aqStats.R?
#'
#'
#' @param data A tibble in rOstluft long format
#' @param statistic Statistical method(s) to apply when aggregating the data.
#'   Can be a simple string with name of the method or a function with one argument.
#'   Or a list with parameter as name and the statistical method as value (function or name of method).
#'   Or a list with parameter as and a list of statisticals methods. All methods must support renaming parameter.
#'   A default statistic for all parameters not in the list, can be defined with the name "default_statistic".
#'   See section Statistical methods and examples
#' @param new_interval New interval. Must be longer than actual interval (not checked)
#' @param data.thresh optional minimum data capture threshold in to use
#' @param max_gap optional maxium Number of consecutive NA values
#' @param rename.parameter optional rename parameter
#' @param percentile The percentile level used when statistic = "percentile". The default is 0.95
#' @param start.date optional start date for padding. Default min date in series floored to the new interval
#' @param end.date optional end date for padding. Default max date in series ceiled to the new interval
#' @param drop.last optional drop the last added time point by padding. Default False, true if no end.date
#'   provided and max date != ceiled max date.
#'
#' @return tibble with resampled data
#'
#' @examples
#' min30 <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2013_Jan.csv",
#'                      package = "rOstluft.data", mustWork = TRUE)
#'
#' airmo_min30 <- read_airmo_csv(min30)
#'
#' # filter volume concenctrations, only use mass concentrations
#' airmo_min30 <- dplyr::filter(airmo_min30, !(.data$unit == "ppb" | .data$unit == "ppm"))
#'
#' d1_statistics <- list(
#'   "default_statistic" = "drop",
#'   "Hr" = "mean",
#'   "RainDur" = "sum",
#'   "O3" = list("mean", "max", "min", "n")
#' )
#' resample(airmo_min30, d1_statistics, "d1", data.thresh = 0.8)
#'
#' # Note: wind parameters don't support multiple methods via list!
#' h1_statistics <- list(
#'   "default_statistic" = "drop",
#'   "WD" = "wind.direction",
#'   "WVs" = "wind.speed_scalar",
#'   "WVv" = "wind.speed_vector",
#'   "RainDur" = "sum",
#'   "NO" = list("coverage", "mean")
#' )
#' resample(airmo_min30, h1_statistics, "h1", data.thresh = 0.8)
#'
#' # Note: all resulting values should be NA -> gap is to big (480 * min30 = 10 days)
#' y1_statistics <- list(
#'   "default_statistic" = "drop",
#'   "O3" = list("mean", "perc98", "n", "max", "min")
#' )
#' resample(airmo_min30, y1_statistics, "y1", max_gap = 480)
#'
#' @export
resample <- function(data, statistic = "mean", new_interval, data.thresh = NULL, max_gap = NULL,
                     rename.parameter = TRUE, percentile = 0.95,
                     start.date = NULL, end.date = NULL, drop.last = FALSE) {

  # build helper function to get the statistical method for the parameter
  if (!is.list(statistic)) {
    get_statistic_for_serie <- function(parameter) {
      statistic
    }
    is_wind_parameter <- FALSE
  } else {
    default_statistic <- getElement2(statistic, "default_statistic", "mean")
    get_statistic_for_serie <- function(parameter) {
      getElement2(statistic, as.character(parameter), default_statistic)
    }
    is_wind_parameter <- purrr::map_lgl(statistic, ~is.character(.) && stringr::str_starts(., "wind."))
  }

  # separate wind data from rest
  wind.data <- NULL
  if (any(is_wind_parameter)) {
    wind_parameters <- statistic[is_wind_parameter]

    if (length(wind_parameters) != 3) {
      stop("resampling wind expects that all 3 components (speed_scalar, speed_vector, direction) are defined.")
    }

    data <- cut_wind_data(data, names(wind_parameters))

    if (!is.null(data$wind)) {
      wind.data <- resample_wind(data$wind, wind_parameters, new_interval = new_interval, data.thresh = data.thresh,
                                 start.date = start.date, end.date = end.date, drop.last = drop.last, max_gap = max_gap)
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
    data <-  purrr::map2(data, statistic_list, resample_series, new_interval = new_interval, data.thresh = data.thresh,
                         start.date = start.date, end.date = end.date, drop.last = drop.last,
                         rename.parameter = rename.parameter, percentile = percentile, max_gap = max_gap)
  }

  bind_rows_with_factor_columns(!!!data, !!!wind.data)
}

#' @title resampling a serie
#' @param serie a tibble in rOstluft long format containing exactly one serie
#'
#' @return tibble with resampled series
#'
#' @keywords internal
resample_series <- function(serie, statistic = "mean", new_interval = "d1", data.thresh = NULL,
                            start.date = NULL, end.date = NULL, drop.last = FALSE, rename.parameter = FALSE,
                            percentile = 0.95, max_gap = NULL) {

  interval_converted <- convert_interval(new_interval)
  old_interval <- dplyr::first(serie$interval)


  if (is.null(start.date)) {
    start.date <- lubridate::floor_date(min(serie$starttime), interval_converted)
  }

  if (is.null(end.date)) {
    end.max <- max(serie$starttime)
    end.date <- lubridate::ceiling_date(end.max, interval_converted)
    drop.last <- (end.max != end.date)  #TODO Decide if we should always drop last in this case
  }

  # TODO optional parameter to skip this
  serie <- pad_serie(serie, start.date, end.date, drop.last)


  serie <- dplyr::group_by(serie,
                           starttime = lubridate::floor_date(.data$starttime, interval_converted),
                           .data$site, .data$parameter, .data$interval, .data$unit
  )

  if (is.list(statistic)) {
    no_rename <- purrr::map_lgl(statistic, ~ !(is.character(.) && !is.null(statistic_lookup[[., "rename"]])))
    if ((length(statistic) > 1 && isFALSE(rename.parameter)) || sum(no_rename) > 1) {
      stop("resampling one serie with multiple statistics without renaming")
    }

    serie <- purrr::map(statistic, resample_apply_statistic, serie = serie, new_interval = new_interval,
                        old_interval = old_interval, data.thresh = data.thresh, rename.parameter = rename.parameter ,
                        percentile = percentile, max_gap = max_gap)

    serie <- bind_rows_with_factor_columns(!!!serie)
  } else {
    serie <- resample_apply_statistic(statistic, serie = serie,  new_interval = new_interval,
                                      old_interval = old_interval, data.thresh = data.thresh,
                                      rename.parameter = rename.parameter , percentile = percentile, max_gap = max_gap)
  }

  serie
}

resample_apply_statistic <- function(statistic, serie, new_interval, old_interval, data.thresh = NULL,
                                     rename.parameter = FALSE, percentile = 0.95, max_gap = NULL) {

  if (rlang::is_character(statistic) && statistic == "drop") {
    return(dplyr::ungroup(serie)[0,])
  }

  FUN <- statistic_fun_factory(statistic, threshold =  data.thresh, percentile = percentile, max_gap = max_gap)
  serie <- dplyr::summarise(serie, value = FUN(.data$value))
  serie <- dplyr::ungroup(serie) # needed if we called resample_serie with grouped series

  #TODO could use forcats::fct_recode for Performance?
  serie <- dplyr::mutate(serie, interval = forcats::as_factor(new_interval))

  if (isTRUE(rename.parameter) && is.character(statistic) && !is.null(statistic_lookup[[statistic, "rename"]])) {
    rename_vars <- list(
      parameter = as.character(dplyr::first(serie$parameter)),
      basis_interval = old_interval,
      interval = new_interval
    )
    parameter <- stringr::str_interp(statistic_lookup[[statistic, "rename"]], rename_vars)
    serie$parameter <- forcats::as_factor(parameter)
  }


  if (is.character(statistic) && !is.null(statistic_lookup[[statistic, "new_unit"]])){
    serie$unit <- forcats::as_factor(statistic_lookup[[statistic, "new_unit"]])
  }

  serie
}

#' Resamples wind data
#'
#' @return tibble with resampled wind data
#'
#' @keywords internal
resample_wind <- function(data, statistic, new_interval = "d1", data.thresh = NULL, start.date = NULL, end.date = NULL,
                          drop.last = FALSE, max_gap = NULL) {

  data <- dplyr::group_split(data, .data$site, .data$interval)
  data <- purrr::map(data, resample_wind_site, statistic = statistic, new_interval = new_interval,
                     data.thresh = data.thresh, start.date = start.date, end.date = end.date, drop.last = drop.last,
                     max_gap = NULL)

  data
}


#' resamples wind
#'
#' @param data.site wind data for one specific site. should only contain wind speed and direction
#'
#' @return tibble with resampled wind speed and direction
#' @keywords internal
resample_wind_site <- function(data.site, statistic, new_interval = "d1", data.thresh = NULL, start.date = NULL,
                               end.date = NULL, drop.last = FALSE, max_gap = NULL) {

  interval_converted <- convert_interval(new_interval)
  old_interval <- dplyr::first(data.site$interval)

  if (is.null(start.date)) {
    start.date <- lubridate::floor_date(min(data.site$starttime), interval_converted)
  }

  if (is.null(end.date)) {
    end.max <- max(data.site$starttime)
    end.date <- lubridate::ceiling_date(end.max, interval_converted)
    drop.last <- (end.max != end.date)  #TODO Decide if we should always drop last in this case
  }

  FUN <- statistic_fun_factory("mean", threshold = data.thresh, max_gap = max_gap)

  groups <- dplyr::group_by(data.site, .data$parameter, .data$unit)
  keys <- dplyr::group_keys(groups)
  site <- dplyr::first(data.site$site)
  groups <- dplyr::group_split(groups)

  groups <- purrr::map(groups, pad_serie, start.date, end.date, drop.last)
  groups <- rlang::set_names(groups, statistic[as.character(keys$parameter)])

  lookup_param <- rlang::set_names(names(statistic), statistic)

  ws <- groups$wind.speed_scalar
  wv <- groups$wind.speed_vector
  wd <- groups$wind.direction

  # substitute scalar with vector speed
  if (is.null(ws) && !is.null(wv)) {
    ws <- wv
  }

  # substitute vector with scalar speed
  if (is.null(wv) && !is.null(ws)) {
    wv <- ws
  }

  if (!is.null(wv) && !is.null(wd)) {
    wind_components <- tibble::tibble(
      starttime = lubridate::floor_date(ws$starttime, interval_converted),
      U = wv$value * sin(2 * pi * wd$value / 360),
      V = wv$value * cos(2 * pi * wd$value / 360)
    )

    wind_components <- dplyr::group_by(wind_components, .data$starttime)
    wind_components <- dplyr::summarise(wind_components, U = FUN(.data$U), V = FUN(.data$V))
    wind_components <- dplyr::mutate(wind_components,
                                     wd = (atan2(.data$U, .data$V) * 360 / 2 / pi) %% 360,
                                     wv = sqrt(.data$U^2 + .data$V^2))

    wv <- tibble::tibble(
      starttime = wind_components$starttime,
      site = dplyr::first(wv$site),
      parameter = forcats::as_factor(lookup_param[["wind.speed_vector"]]),
      interval = forcats::as_factor(new_interval),
      unit = dplyr::first(wv$unit),
      value = wind_components$wv
    )

    wd <- tibble::tibble(
      starttime = wind_components$starttime,
      site = dplyr::first(wd$site),
      parameter = forcats::as_factor(lookup_param[["wind.direction"]]),
      interval = forcats::as_factor(new_interval),
      unit = dplyr::first(wd$unit),
      value = wind_components$wd
    )
  } else {
    wv <- wd <- data.site[0, ]
  }

  if (!is.null(ws)) {
    ws <- dplyr::group_by(ws,
                    starttime = lubridate::floor_date(.data$starttime, interval_converted),
                    .data$site, .data$parameter, .data$interval, .data$unit
    )
    ws <- dplyr::summarise(ws, value = FUN(.data$value))
    ws <- dplyr::ungroup(ws)
    ws <- dplyr::mutate(ws, interval = forcats::as_factor(new_interval),
                        parameter = forcats::as_factor(lookup_param[["wind.speed_scalar"]]))
  } else {
    ws <- data.site[0, ]
  }

  bind_rows_with_factor_columns(ws, wv, wd)
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



#' Cut wind data
#'
#' This function splits off the wind parameters
#'
#' TODO: create an all purpuse function?
#'
#' @param data tibble with input data
#' @param wind_parameters Character Vector of Wind parameters. Default wind_parameters = c("WD", "WVv")
#'
#' @return named list: $wind all wind parameters, $others everything else
#'
#' @keywords internal
cut_wind_data <- function(data, wind_parameters = c("WD", "WVv")) {
  data <- dplyr::group_by(data, isWind= .data$parameter %in% wind_parameters)
  keys <- dplyr::group_keys(data)
  data <- dplyr::group_split(data, keep = FALSE)
  mapping <- list("TRUE" = "wind", "FALSE" = "others")
  rlang::set_names(data, mapping[as.character(keys$isWind)])  # FALSE before TRUE, order don't matters
}
