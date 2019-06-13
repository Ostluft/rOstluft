#' Calculates weighted arithmetic means to converted shifted intervals to regular
#'
#' @description
#' Sometimes records (e.g. measurements from passive samplers or miniDOAS) provide values representative for time
#' intervals such as starting from odd start times to odd end times (e.g. 09:58 to 10:08 or 20 Feb to 06 March) with
#' respect to full time intervals  for mean intervals (e.g. 09:00 to 10:00 or 01 Feb to 28 Feb). To make such time
#' series intercomparable and provide a standardized way of dealing with aggregated data, wmean_full_interval()
#' provides a method to average a data.frame containing start- and end-time information to full time intervals based
#' on stats::weighted.mean().
#'
#' @section Restrictions:
#' * input data has to contain evenly spaced time series (eg. 10min interval)
#' * the output intervals has to be greater or equal (>=) than the input interval
#' * no handling of missing data threshold. One existing value is enough
#'
#' @param data data.frame for averaging; df has to be in long format and contain a start- and end-time
#'   column of class POSIXct (arbitrarily named)
#' @param starttime name of starttime column as symbol or string
#' @param endtime name of endtime column  as symbol or string
#' @param value name of column containing values to be averaged as symbol or string
#' @param ... columns containing values for grouping (passed to `dplyr::group_by()`) when calculating
#'   weighted means (e.g. for different measurement parameters). Columns not explicitly passed are dropped
#' @param interval specifying the output interval for averaging as `lubridate::Period` (e.g. lubridate::hours(1))
#'
#' @return tibble with the starttime, endtime, value and grouping columns and additional the column "n" containing the
#'   sum of weighted intervals within the averaged time interval
#'
#' @keywords statistics
#'
#' @examples
#' fn <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv", package = "rOstluft.data")
#' data <- read_airmo_csv(fn, time_shift = lubridate::period(20, "minutes"))
#'
#' df <- pluck_parameter(data, "CO") %>%
#'   pluck_unit(unit, "ppm") %>%
#'   dplyr::mutate(endtime = .data$starttime + lubridate::hours(1)) %>%
#'   dplyr::select(-interval)
#'
#' wmean_shifted(df, site, parameter, unit, interval = lubridate::hours(1))
#'
#' wmean_shifted(df, site, parameter, unit, interval = lubridate::days(1))
#'
#' wmean_shifted(df, site, parameter, unit, interval = lubridate::period(1, "month"))
wmean_shifted <- function(data, ..., starttime = "starttime", endtime = "endtime", value = "value",
                          interval = lubridate::hours(1)) {

  # symbolize arguments
  starttime <- rlang::ensym(starttime)
  endtime <- rlang::ensym(endtime)
  value <- rlang::ensym(value)
  dots <- rlang::ensyms(...)

  # normalize naming
  data <- dplyr::rename(data, starttime = !!starttime, endtime = !!endtime, value = !!value)

  data <- dplyr::mutate(data,
    start_interval = lubridate::floor_date(.data$starttime, unit = interval),
    end_interval = .data$start_interval + interval,
    w = 1
  )

  # split the overlapping measurements off
  data <- dplyr::group_by(data, isOverlapping =  .data$end_interval < .data$endtime)
  keys <- dplyr::group_keys(data)
  data <- dplyr::group_split(data, keep = FALSE)
  mapping <- list("TRUE" = "overlaps", "FALSE" = "complete")
  data <- rlang::set_names(data, mapping[as.character(keys$isOverlapping)])

  if (!is.null(data$overlaps)) {
    measurement_seconds <- as.numeric(data$overlaps$endtime[1] - data$overlaps$starttime[1], units = "secs")

    # pass the right overlapping fraction to next interval and calculate w
    data$right <- dplyr::mutate(data$overlaps,
      start_interval = .data$end_interval,
      end_interval = .data$start_interval + interval,
      w = as.numeric(.data$endtime - .data$start_interval, units = "secs") / measurement_seconds
    )

    # calculate w for the left side of the overlapping measurement
    data$overlaps <- dplyr::mutate(data$overlaps,
      w = as.numeric(.data$end_interval - .data$starttime, units = "secs") / measurement_seconds
    )
  }

  data <- dplyr::bind_rows(!!!data)

  # finally caclulate the weighted mean
  data <- dplyr::group_by(data, .data$start_interval, .data$end_interval, !!!dots)
  data <- dplyr::summarise(data,
    mean = stats::weighted.mean(.data$value, .data$w, na.rm = TRUE),
    n = sum(.data$w, na.rm = TRUE)
  )

  data <- dplyr::ungroup(data)

  # revert naming normalization
  dplyr::rename(data, !!starttime := .data$start_interval, !!endtime := .data$end_interval, !!value := .data$mean)
}



fill_ps <- function(data, interval = "h1") {
  interval_converted <- convert_interval(interval)

  data <- dplyr::mutate(data,
    starttime = lubridate::floor_date(.data$starttime, interval_converted),
    endtime  = lubridate::floor_date(.data$endtime, interval_converted),
  )

  data <- dplyr::group_nest(data, .data$site, .data$parameter, .data$unit, .key = "serie")
  data <- dplyr::mutate(data, serie = purrr::map(.data$serie, fill_ps_serie, interval_converted))
  data <- tidyr::unnest(data, .data$serie)
  data <- dplyr::mutate(data, interval = factor(interval))

  dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
}


fill_ps_serie <- function(serie, interval) {
  purrr::pmap_dfr(serie, fill_ps_messung, interval)
}



fill_ps_messung <- function(starttime, endtime, value, interval) {
  tibble::tibble(
    starttime = seq(starttime, endtime, interval),
    value = value
  ) %>% dplyr::slice(-dplyr::n())
}

