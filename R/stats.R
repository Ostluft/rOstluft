
#' Detects the interval of the data data
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


#' Pads data data to complete years
#'
#' TODO: Export? could be useful for an user
#'
#' @param data input data in rolf format
#'
#' @return padded data in rolf format
#'
#' @keywords internal
pad_input <- function(data) {
  start_date <- lubridate::floor_date(min(data$starttime), "year")
  end_max <- max(data$starttime)
  end_date <- lubridate::ceiling_date(end_max, "year")
  drop_last <- (end_max != end_date)
  pad(data, start_date, end_date, drop_last)
}


#' Prepares data for stats calculations
#'
#' Most indicators are calculated from mass concentrations. This functions converts all volume concentrations to mass
#' concentration with the exception of NOx. The argument keep_ppb allows to keep the volume concentrations
#'
#' @param data input data in rolf format
#' @param keep_ppb usually volume concentrations are not used for analyses and are dropped with the exception of NOx.
#'   When FALSE drops parameters with ppb/ppm units except NOx. Default FALSE
#'
#' @return tibble in rolf format containing mass concentrations
#' @export
calculate_mass_concentrations <- function(data, keep_ppb = FALSE) {
  conversions <- tibble::tribble(
    ~parameter, ~from, ~to,
    "CO", "ppm", "mg/m3",
    "NO", "ppb", "\u00b5g/m3",
    "O3", "ppb", "\u00b5g/m3",
    "NO2", "ppb", "\u00b5g/m3",
    "SO2", "ppb", "\u00b5g/m3"
  )

  if (isFALSE(keep_ppb)) {
    method = "replace"
  } else {
    method = "append"
  }

  data <- convert_conc_multiple(data, conversions, method)
}


#' Calculates most common stats
#'
#' @description
#' Calculates the most common used stats from min10 or min30 data values for every parameter:
#'
#' * input > h1, d1, m1, y1: Averages (wind vector and scalar, RainDur Sum):
#' * input > y1: min, max, n, perc95 (perc98, perc02 for O3, only n for RainDur)
#' * input > m1: min, max (no stats for RainDur)
#' * input > d1: min, max (no stats for RainDur)
#' * d1 > y1: min, max, n (no stats for RainDur)
#' * d1 > m1: min, max (no stats for RainDur)
#'
#' @param data input data in rolf format
#'
#' @return list with stats for h1, d1, m1, y1
#' @export
calculate_stats <- function(data) {
  # data > h1, d1, m1, y1
  averages <- list(
    "default_statistic" = "mean",
    "WD" = "wind.direction",
    "WVs" = "wind.speed_scalar",
    "WVv" = "wind.speed_vector",
    "RainDur" = "sum"
  )

  stats_y1_from_inp <- list(
    "default_statistic" =  list("min", "max", "n", "perc95"),
    "RainDur" = "n",
    "O3" = list("min", "max", "n", "perc98", "perc02")
  )

  stats_y1_from_d1 <- list(
    "default_statistic" =  list("min", "max", "n"),
    "RainDur" = "drop"
  )

  # rest: data > d1, m1  | d1 > m1
  stats_min_max <- list(
    "default_statistic" = list("min", "max"),
    "RainDur" = "drop"
  )

  ################# CODE START ##################################
  interval <- detect_interval(data)

  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))

  # pad the data one time manual instead of every time
  data <- pad_input(data)

  # average all the data for different intervals as basis
  h1 <- resample(data, averages, "h1", data_thresh = 0.8, skip_padding = TRUE)
  d1 <- resample(data, averages, "d1", data_thresh = 0.8, skip_padding = TRUE)
  m1 <- resample(data, averages, "m1", data_thresh = 0.8, skip_padding = TRUE)
  y1 <- resample(data, averages, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)

  # calculate the different stats
  y1_from_inp <- resample(data, stats_y1_from_inp, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)
  y1_from_d1 <- resample(d1, stats_y1_from_d1, "y1", data_thresh = 0.8, skip_padding = TRUE)

  # collect some min max
  d1_from_inp <- resample(data, stats_min_max, "d1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_inp <- resample(data, stats_min_max, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_d1 <- resample(d1, stats_min_max, "m1", data_thresh = 0.8, skip_padding = TRUE)

  res <- list(
    h1 = h1,
    d1 = bind_rows_with_factor_columns(d1, d1_from_inp),
    m1 = bind_rows_with_factor_columns(m1, m1_from_inp, m1_from_d1),
    y1 = bind_rows_with_factor_columns(y1, y1_from_inp, y1_from_d1)
  )

  res
}


#' Calculates exceedance of LRV limits
#'
#' @description
#' Calculates the number of exceedances of the limits defined in the
#' [LRV](https://www.admin.ch/opc/de/classified-compilation/19850321/index.html):
#'
#' * h1 > y1: n>120 of O3
#' * d1 > y1: n>100 of SO2, n>80 of NO2, n>8 of CO, n>50 of PM10, n>25 of PM2.5, n>120 of O3_max_h1 as O3_nb_d1_h1>120
#'
#' @param data input data in rolf format should contain 10min or 30min data and one of CO, NO2, SO2, O3, PM10, PM2.5 as
#'   mass concentration
#' @param quiet Instead of stopping if no parameter is found return an empty frame. Default FALSE
#'
#' @return list with m1 and y1 stats
#' @export
calculate_LRV <- function(data, quiet = FALSE) {
  averages_h1 <- list(
    "default_statistic" = "drop",
    "O3" = "mean"
  )

  averages_d1 <- list(
    "default_statistic" = "drop",
    "CO" = "mean",
    "PM10" = "mean",
    "PM2.5" = "mean",
    "NO2" = "mean",
    "SO2" = "mean"
  )

  O3_d1_on_h1 <- list(
    "default_statistic" = "drop",
    "O3" = list("max", "n>120")
  )

  limits_h1 <- list(
    "default_statistic" = "drop",
    "O3" = list("max", "n>120")
  )

  limits_d1 <- list(
    "default_statistic" = "drop",
    "CO" = "n>8",
    "PM10" = "n>50",
    "PM2.5" = "n>25",
    "NO2" = "n>80",
    "SO2" = "n>100",
    "O3_max_h1" = "n>120"
  )

  # keep only mass concentrations
  data <- dplyr::filter(data, .data$unit == "\u00b5g/m3" | .data$unit == "mg/m3")

  if (nrow(data) == 0) {
    if (isFALSE(quiet)) {
      stop("data contains no parameters in mass concentrations")
    } else {
      return(data[0, ])
    }
  }

  interval <- detect_interval(data)
  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))

  # pad the data one time manual instead of every time
  data <- pad_input(data)

  h1 <- resample(data, averages_h1, "h1", data_thresh = 0.8, skip_padding = TRUE)
  d1 <- resample(data, averages_d1, "d1", data_thresh = 0.8, skip_padding = TRUE)
  d1_O3 <- resample(h1, O3_d1_on_h1, "d1", data_thresh = 0.8, skip_padding = TRUE)
  d1 <- bind_rows_with_factor_columns(d1, d1_O3)

  m1_from_h1 <- resample(h1, limits_h1, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_d1 <- resample(d1, limits_d1, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_d1 <- dplyr::mutate(m1_from_d1,
                              parameter = dplyr::recode(.data$parameter, "O3_max_h1_nb_d1>120" = "O3_nb_d1_mit_h1>120")
  )

  y1_from_h1 <- resample(h1, limits_h1, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)
  y1_from_d1 <- resample(d1, limits_d1, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)
  y1_from_d1 <- dplyr::mutate(y1_from_d1,
                              parameter = dplyr::recode(.data$parameter, "O3_max_h1_nb_d1>120" = "O3_nb_d1_mit_h1>120")
  )

  list(
    m1 = bind_rows_with_factor_columns(m1_from_h1, m1_from_d1),
    y1 = bind_rows_with_factor_columns(y1_from_h1, y1_from_d1)
  )
}


#' Calculates a lot of O3 indicators
#'
#' @description
#' calculates the following direct statistics:
#'
#' * h1 > d1: "n>160", "n>180", "n>200", "n>240", "max", "n"
#' * h1 > m1: "n>160", "n>180", "n>200", "n>240", "max", "n"
#' * h1 > y1: "n>160", "n>180", "n>200", "n>240", "max", "n"
#' * d1 > m1: "n>65"
#' * d1 > y1: "n>65"
#' * h1 > m6,m4-m9: 7h mean from 9:00 - 16:00 CET, AOT40 sum ppb value > 40 from 8:00 - 20:00 CET
#'
#' And following two step statistics (number of days with hour values > xxx):
#'
#' * h1 > m1: O3_nb_d1_h1>160, O3_nb_d1_h1>180, O3_nb_d1_h1>200, O3_nb_d1_h1>240
#' * h1 > y1: O3_nb_d1_h1>160, O3_nb_d1_h1>180, O3_nb_d1_h1>200, O3_nb_d1_h1>240
#'
#' @param data input data in rolf format should contain 10min or 30min data of O3 in µg/m3
#' @param quiet Instead of stopping if no parameter is found return an empty frame. Default FALSE
#'
#' @return list with d1, m1, y1 and
#' @export
calculate_O3 <- function(data, quiet = FALSE) {
  # keep only O3 in mass concentrations
  data <- dplyr::filter(data, .data$parameter == "O3", .data$unit == "\u00b5g/m3")

  if (nrow(data) == 0) {
    if (isFALSE(quiet)) {
      stop("data contains no O3 in \u00b5g/m3")
    } else {
      return(data[0, ])
    }
  }

  interval <- detect_interval(data)
  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))

  # prepare the means
  data <- pad_input(data)
  h1 <- resample(data, "mean", "h1", data_thresh = 0.8, skip_padding = TRUE)
  d1 <- resample(data, "mean", "d1", data_thresh = 0.8, skip_padding = TRUE)

  # simple stats
  indicators_h1 = list("O3" = list("n>160", "n>180", "n>200", "n>240", "max", "n"))
  d1_from_h1 <- resample(h1, indicators_h1, "d1", data_thresh = 0.8, skip_padding = TRUE)

  m1_from_h1 <- resample(h1, indicators_h1, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_d1 <- resample(d1, "n>65", "m1", data_thresh = 0.8, skip_padding = TRUE)

  y1_from_h1 <- resample(h1, indicators_h1, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)
  y1_from_d1 <- resample(d1, "n>65", "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)

  # number of days with hours >xxx
  indicators_nb_d1_h1 = list(
    "default_statistic" = "drop",
    "O3_max_h1" = list("n>160", "n>180", "n>200", "n>240")
  )

  mapping <- list(
    "O3_max_h1_nb_d1>160" = "O3_nb_d1_mit_h1>160",
    "O3_max_h1_nb_d1>180" = "O3_nb_d1_mit_h1>180",
    "O3_max_h1_nb_d1>200" = "O3_nb_d1_mit_h1>200",
    "O3_max_h1_nb_d1>240" = "O3_nb_d1_mit_h1>240"
  )

  m1_nb_d1_h1 <- resample(d1_from_h1, indicators_nb_d1_h1, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_nb_d1_h1 <- dplyr::mutate(m1_nb_d1_h1,
                               parameter = dplyr::recode(.data$parameter, !!!mapping)
  )

  y1_nb_d1_h1 <- resample(d1_from_h1, indicators_nb_d1_h1, "y1", data_thresh = 0.8, max_gap = max_gap,
                          skip_padding = TRUE)
  y1_nb_d1_h1 <- dplyr::mutate(y1_nb_d1_h1,
                               parameter = dplyr::recode(.data$parameter, !!!mapping)
  )

  # calculate hours and month to select the correct data for AOT40 (sum ppb beween 8:00 - 20:00 CET) and
  # mean_7h (between 09:00 - 16:00 CET) from April until September
  AOT40 <- dplyr::mutate(h1,
                         hour = lubridate::hour(.data$starttime),
                         month = lubridate::month(.data$starttime)
  )

  mean_7h <- dplyr::filter(AOT40, dplyr::between(.data$month, 4, 9), dplyr::between(.data$hour, 9, 15))
  mean_7h <- dplyr::select(mean_7h, "starttime", "site", "parameter", "interval", "unit", "value")
  mean_7h <- resample(mean_7h, "mean", "y1", skip_padding = TRUE)
  mean_7h <- dplyr::mutate(mean_7h, parameter = as.factor("O3_h709001600"))

  AOT40 <- convert_conc(AOT40, "O3", "\u00b5g/m3", "ppb")
  AOT40 <- dplyr::filter(AOT40, dplyr::between(.data$month, 4, 9), dplyr::between(.data$hour, 8, 19))
  AOT40 <- dplyr::select(AOT40, "starttime", "site", "parameter", "interval", "unit", "value")
  AOT40 <- resample(AOT40, "AOT40k", "y1", skip_padding = TRUE)

  list(
    d1 = d1_from_h1,
    m1 = bind_rows_with_factor_columns(m1_from_h1, m1_from_d1, m1_nb_d1_h1),
    y1 = bind_rows_with_factor_columns(y1_from_h1, y1_from_d1, y1_nb_d1_h1, mean_7h, AOT40)
  )
}



#' Calculates CO 8 hour rolling statistics
#'
#' @description
#' This function uses openair::rollingMean to calculate the 8 hour rolling Mean. When openair cannot be loaded, an
#' error is thrown.
#'
#' Statistics:
#'
#' * h8gl left aligned
#' * max h8gl > y1
#'
#'
#' @param data input data in rolf format should contain 10min or 30min data of CO in mg/m3
#' @param quiet Instead of stopping if no parameter is found return an empty frame. Default FALSE
#'
#' @return list with h8gl and y1 stats
#' @export
calculate_CO_h8gl <- function(data, quiet = FALSE) {
  if (isFALSE(requireNamespace("openair"))) {
    stop("Package openair is needed")
  }

  # keep only mass concentrations
  data <- dplyr::filter(data, .data$parameter == "CO", .data$unit == "mg/m3")

  if (nrow(data) == 0) {
    if (isFALSE(quiet)) {
      stop("data contains no CO in mg/m3")
    } else {
      return(data[0, ])
    }
  }

  interval <- detect_interval(data)
  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))

  # calculate the rolling mean in openair
  data <- resample(data, "mean", "h1", data_thresh = 0.8)
  data <- rolf_to_openair_single(data)
  data <- openair::rollingMean(data, pollutant = "CO", width = 8, new.name = "CO", data.thresh = 80, align = "left")
  data <- openair_to_rolf(data, interval = "h8gl")

  y1 <- resample(data, "max", "y1", data_thresh = 0.8, max_gap = max_gap)

  list(
    h8gl = data,
    y1 = y1
  )
}

#' Calculates O3 8 hour rolling statistics
#'
#' @description
#' This function uses openair::rollingMean to calculate the 8 hour rolling Mean. When openair cannot be loaded, an
#' error is thrown.
#'
#' Statistics:
#'
#' * h8gl left aligned
#' * max, n>100, n>120, n>200 of h8gl > y1
#'
#'
#' @param data input data in rolf format should contain 10min or 30min data of O3 in µg/m3
#' @param quiet Instead of stopping if no parameter is found return an empty frame. Default FALSE
#'
#' @return list with h8gl and y1 stats
#' @export
calculate_O3_h8gl <- function(data, quiet = FALSE) {
  if (isFALSE(requireNamespace("openair"))) {
    stop("Package openair is needed")
  }

  # keep only mass concentrations
  data <- dplyr::filter(data, .data$parameter == "O3", .data$unit == "\u00b5g/m3")

  if (nrow(data) == 0) {
    if (isFALSE(quiet)) {
      stop("data contains no O3 in \u00b5g/m3")
    } else {
      return(data[0, ])
    }
  }

  interval <- detect_interval(data)
  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))

  # calculate the rolling mean in openair
  data <- resample(data, "mean", "h1", data_thresh = 0.8)
  data <- rolf_to_openair_single(data)
  data <- openair::rollingMean(data, pollutant = "O3", width = 8, new.name = "O3", data.thresh = 80, align = "left")
  data <- openair_to_rolf(data, interval = "h8gl")

  y1 <- resample(data, list("O3"= list("max", "n>100", "n>120", "n>200")), "y1", data_thresh = 0.8, max_gap = max_gap)

  list(
    h8gl = data,
    y1 = y1
  )
}


