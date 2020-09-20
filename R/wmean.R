#' Calculates weighted arithmetic means of irregular data series
#'
#' @description
#' Sometimes records (e.g. measurements from passive samplers or miniDOAS) provide values representative irregular time
#' intervals such as starting from odd start times to odd end times. For example 09:58 to 10:08 or 20 Feb to 06 March.
#' This function interpolates the irregular data on a standard interval. It handles down- and upsampling correct.
#'
#' @section Caution:
#' * Removes NA values to correctly calculate data availability for each interval
#' * output data isn't padded
#'
#' @param data data.frame for averaging; df has to be in long format and contain a start- and end-time
#'   column of class POSIXct (arbitrarily named)
#' @param ... columns containing values for grouping (passed to `dplyr::group_by()`) when calculating
#'   weighted means (e.g. for different measurement parameters). Columns not explicitly passed are dropped
#' @param starttime name of starttime column as symbol or string
#' @param endtime name of endtime column  as symbol or string
#' @param value name of column containing values to be averaged as symbol or string
#' @param interval specifying the output interval for averaging as string
#'
#' @return tibble with the starttime, endtime, value and grouping columns and additional the column "n" containing the
#'   sum of weighted intervals within the averaged time interval (data availability in interval, 1 = 100\%)
#'
#' @export
#'
#' @examples
#' fn <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv", package = "rOstluft.data")
#' ps_fn <- system.file("extdata", "NO2_PS.rds", package = "rOstluft.data")
#' data <- read_airmo_csv(fn, time_shift = lubridate::period(25, "minutes"))
#' data_ps <- readRDS(ps_fn) %>% pluck_site("Zch_Stampfenbachstrasse")
#'
#' df <- pluck_parameter(data, "CO") %>%
#'   pluck_unit("ppm") %>%
#'   dplyr::mutate(endtime = .data$starttime + lubridate::hours(1)) %>%
#'   dplyr::select(-interval)
#'
#' wmean(df, site, parameter, unit, interval = "h1")
#'
#' wmean(df, site, parameter, unit, interval = "m1")
#'
#' wmean(df, site, parameter, unit, interval = "y1")
#'
#' wmean(data_ps, site, parameter, unit, interval = "d1")
#'
#' wmean(data_ps, site, parameter, unit, interval = "1 week")
#'
#' wmean(data_ps, site, parameter, unit, interval = "m1")
#'
#' wmean(data_ps, site, parameter, unit, interval = "y1")
#'
wmean <- function(data, ..., starttime = "starttime", endtime = "endtime", value = "value",
                          interval = "h1") {
  # symbolize arguments
  starttime <- rlang::ensym(starttime)
  endtime <- rlang::ensym(endtime)
  value <- rlang::ensym(value)
  dots <- rlang::ensyms(...)
  interval <- convert_interval(interval)

  # normalize naming
  data <- dplyr::rename(data, starttime_ = !!starttime, endtime_ = !!endtime, value_ = !!value)

  data <- dplyr::mutate(data,
    start_interval_ = lubridate::floor_date(.data$starttime_, unit = interval),
    end_interval_ = .data$start_interval_ + lubridate::period(interval)
  )

  # split the overlapping measurements off
  data <- cut_on_condition(data, .data$end_interval_ < .data$endtime_, c("TRUE" = "overlaps", "FALSE" = "complete"))

  # calculate the weight for measurements complete in one interval
  if (rlang::has_name(data, "complete")) {
    data$complete <- dplyr::mutate(data$complete,
      w = as.numeric(.data$endtime_ - .data$starttime_, units = "secs") /
        as.numeric(.data$end_interval_ - .data$start_interval_, units = "secs")
    )
  }

  if (rlang::has_name(data, "overlaps")) {
    # we only checked if a measurements overlaps a period, not if it overlaps multiple periods.
    # if we floor the endtime and is the same as end_interval the measurement overlaps only one measurement
    data$overlaps <- dplyr::mutate(data$overlaps,
      endtime_interval_ = lubridate::floor_date(.data$endtime_, unit = interval),
    )
    data$overlaps <- cut_on_condition(data$overlaps, .data$endtime_interval_ == .data$end_interval_,
                                      c("TRUE" = "one", "FALSE" = "multi"))

    # handle the measurement only overlapping one measurement and split them in left and right

    if (rlang::has_name(data$overlaps, "one")) {
      # calculate w for the left side of the overlapping measurement
      data$left <- dplyr::mutate(data$overlaps$one,
        w = as.numeric(.data$end_interval_ - .data$starttime_, units = "secs") /
           as.numeric(.data$end_interval_ - .data$start_interval_, units = "secs")
      )

      # pass the right overlapping fraction to next interval and calculate w
      data$right <- dplyr::mutate(data$overlaps$one,
        start_interval_ = .data$end_interval_,
        end_interval_ = .data$start_interval_ + lubridate::period(interval),
        w = as.numeric(.data$endtime_ - .data$start_interval_, units = "secs") /
           as.numeric(.data$end_interval_ - .data$start_interval_, units = "secs")
      )

    }

    if (rlang::has_name(data$overlaps, "multi")) {
      data$multi_left <- dplyr::mutate(data$overlaps$multi,
        w = as.numeric(.data$end_interval_ - .data$starttime_, units = "secs") /
           as.numeric(.data$end_interval_ - .data$start_interval_, units = "secs")
      )

      data$multi_right <- dplyr::mutate(data$overlaps$multi,
        start_interval_ = .data$endtime_interval_,
        end_interval_ = .data$start_interval_ + lubridate::period(interval),
        w = as.numeric(.data$endtime_ - .data$start_interval_, units = "secs") /
           as.numeric(.data$end_interval_ - .data$start_interval_, units = "secs")
      )

      data$mutli_middle <- fill_wmean(data$overlaps$multi, !!!dots, interval = interval)
    }
  }

  data[["overlaps"]] <- NULL
  data <- dplyr::bind_rows(!!!data)

  # finally caclulate the weighted mean
  data <- dplyr::group_by(data, .data$start_interval_, .data$end_interval_, !!!dots)
  data <- dplyr::filter(data, !is.na(.data$value_))
  data <- dplyr::summarise(data,
    value_ = stats::weighted.mean(.data$value_, .data$w, na.rm = TRUE),
    n = sum(.data$w, na.rm = TRUE)
  )

  data <- dplyr::ungroup(data)

  # revert naming normalization
  dplyr::rename(data, !!starttime := .data$start_interval_, !!endtime := .data$end_interval_, !!value := .data$value_)
}

#' Fills (upsample) a data frame for wmean
#'
#' @param data input data frame
#' @param ... grouping columns
#' @param interval for upsampling
#'
#' @return upsampled data frame
#' @keywords internal
fill_wmean <- function(data, ..., interval) {

  fill_wmean_serie <- function(serie, interval) {
    purrr::pmap_dfr(serie, fill_wmean_measurement, interval = interval)
  }

  fill_wmean_measurement <- function(end_interval_, endtime_interval_, value_, ..., interval) {
    startime <- seq.POSIXt(end_interval_, endtime_interval_, interval)
    startime <- utils::head(startime, -1)

    tibble::tibble(
      startime = startime,
      start_interval_ = startime,
      endtime_ = startime + lubridate::period(interval),
      end_interval_ = .data$endtime_,
      value_ = value_,
      w = 1
    )
  }

  data <- dplyr::group_nest(data, ..., .key = "serie")
  data <- dplyr::mutate(data, serie = purrr::map(.data$serie, fill_wmean_serie, interval = interval))
  tidyr::unnest(data, .data$serie)
}
