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
    method <- "replace"
  } else {
    method <- "append"
  }

  data <- convert_conc_multiple(data, conversions, method)
}


statstable_default <- function(expand = FALSE) {
  statstable <- tibble::tribble(
    ~parameter, ~statistic, ~from, ~to,
    "CO, NO, NOx, NO2, O3, SO2", "mean", "input", "h1,d1,m1,y1",
    "CO, NO, NOx, NO2, O3, SO2", "max, min, n", "input", "d1,m1,y1",
    "CO, NO, NOx, NO2, SO2", "perc95", "input", "m1,y1",
    "O3", "perc02, perc98", "input", "m1,y1",
    "O3", "max, min, n, n>120, n>160, n>180, n>200, n>240", "h1", "d1,m1,y1",
    "NO2", "max, n, n>200", "h1", "d1,m1,y1",
    "O3", "mean", "h1", "h8gl",
    "CO", "mean", "h1", "h8gl",
    "CO", "max", "h8gl", "y1",
    "O3", "max, n>120, n>160, n>180, n>200, n>240", "h8gl", "y1",
    "CO, NO, NOx, NO2, O3, SO2", "min, max, n", "d1", "m1,y1",
    "CO", "n>8", "d1", "m1,y1",
    "SO2", "n>100", "d1", "m1,y1",
    "NO2", "n>80", "d1", "m1,y1",
    "O3", "n>120, n>160, n>200, n>180, n>240, n>65", "d1", "m1,y1",
    "O3_max_h1", "n>120, n>160, n>180, n>200, n>240", "d1", "m1,y1",
    "O3_98%_min30", "max, n>100", "m1", "y1",
    "EC1.0, EC2.5, PM10, PM2.5, PN", "mean", "input", "h1,d1,m1,y1",
    "EC1.0, EC2.5, PM10, PM2.5, PN", "max, min, n", "input", "d1,m1,y1",
    "EC1.0, EC2.5, PM10, PM2.5, PN", "perc95", "input", "m1,y1",
    "EC1.0, EC2.5, PM10, PM2.5, PN", "min, max, n", "d1", "m1,y1",
    "PM10", "n>50", "d1", "m1,y1",
    "PM2.5", "n>25", "d1", "m1,y1",
    "WD", "wind.direction", "input", "h1,d1,m1,y1",
    "WVs", "wind.speed_scalar", "input", "h1,d1,m1,y1",
    "WVv", "wind.speed_vector", "input", "h1,d1,m1,y1",
    "WD", "n", "input", "d1,m1,y1",
    "WVs", "max, min, n", "input", "d1,m1,y1",
    "WVv", "max, min, n", "input", "d1,m1,y1",
    "Hr, p, StrGlo, T", "mean", "input", "h1,d1,m1,y1",
    "Hr, p", "min,max", "input", "d1",
    "T", "min, max, n", "input", "d1,m1,y1",
    "T", "min, max", "h1", "d1,m1,y1",
    "T", "min, max, n", "d1", "m1,y1",
    "StrGlo", "max, n", "input", "d1,m1,y1",
    "Hr, p", "n", "input", "d1,m1,y1",
    "RainDur, RainSum,  SunDur", "sum", "input", "h1,d1,m1,y1",
    "RainDur, RainSum,  SunDur", "n", "input", "d1,m1,y1",
    "RainDur, RainSum,  SunDur", "max", "d1", "y1"
  )

  if (isTRUE(expand)) {
    statstable <- statstable_expand(statstable)
  }

  statstable
}

calculate_stats <- function(data) {
  statstable <- statstable_default()
  data <- calculate_mass_concentrations(data)
  data <- calculate_statstable(data, statstable)
  data <- bind_rows_with_factor_columns(!!!data)

  # rename days with hours > xxx
  mapping <- list(
    "O3_max_h1_nb_d1>120" = "O3_nb_d1_mit_h1>120",
    "O3_max_h1_nb_d1>160" = "O3_nb_d1_mit_h1>160",
    "O3_max_h1_nb_d1>180" = "O3_nb_d1_mit_h1>180",
    "O3_max_h1_nb_d1>200" = "O3_nb_d1_mit_h1>200",
    "O3_max_h1_nb_d1>240" = "O3_nb_d1_mit_h1>240"
  )

  dplyr::mutate(data, parameter = dplyr::recode(.data$parameter, !!!mapping))
}

#' Calculates AOT40 and 7h mean from 9:00 - 16:00 CET from April until September
#'
#' @param data input data in rolf format should contain 10min or 30min data of O3 in µg/m3
#' @param quiet Instead of stopping if no parameter is found return an empty frame. Default FALSE
#'
#' @return tibble in rolf format containing AOT40 and 7h mean
#' @export
calculate_O3_summer <- function(data, quiet = FALSE) {
  # keep only O3 in mass concentrations
  data <- dplyr::filter(data, .data$parameter == "O3")

  if (nrow(data) == 0) {
    if (isFALSE(quiet)) {
      stop("data contains no O3")
    } else {
      return(data[0, ])
    }
  }

  # convert ppb to ug
  data <- calculate_mass_concentrations(data)
  interval <- detect_interval(data)
  max_gap <- switch(interval, "min10" = 6 * 24 * 10, "min30" = 48 * 10,
                    stop("only supports following intervals: min10, min30"))

  # prepare the means
  data <- pad_year(data)
  h1 <- resample(data, "mean", "h1", data_thresh = 0.8, skip_padding = TRUE)

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

  AOT40 <- convert_conc(AOT40, "O3", "\u00b5g/m3", "ppb")  # convert ug/m3 back to ppb ...
  AOT40 <- dplyr::filter(AOT40, dplyr::between(.data$month, 4, 9), dplyr::between(.data$hour, 8, 19))
  AOT40 <- dplyr::select(AOT40, "starttime", "site", "parameter", "interval", "unit", "value")
  AOT40 <- resample(AOT40, "AOT40k", "y1", skip_padding = TRUE)

  bind_rows_with_factor_columns(mean_7h, AOT40)
}
