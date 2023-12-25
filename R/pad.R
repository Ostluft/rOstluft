#' Pads data data to complete years
#'
#' @param data input data in rolf format
#'
#' @return padded data in rolf format
#'
#' @export
pad_year <- function(data) {
  start_date <- lubridate::floor_date(min(data$starttime), "year")
  end_max <- max(data$starttime)
  end_date <- lubridate::ceiling_date(end_max, "year")
  drop_last <- (end_max != end_date)
  pad(data, start_date, end_date, drop_last)
}


#' Pad data
#'
#' Function to pad out missing time points
#'
#' @param data tibble in rOstluft long format
#' @param start_date optional start date for padding. Default min date in series
#' @param end_date optional end date for padding. Default max date in series
#' @param drop_last optional drop the last added time point by padding. Useful when
#'   resampling and end_date is the first time point of the new interval
#'
#' @return tibble with padded data
#'
#' @export
pad <- function(data, start_date = NULL, end_date = NULL, drop_last = FALSE) {
  data.grouped <- dplyr::group_by(data, .data$site, .data$parameter, .data$interval, .data$unit)
  data.grouped <- dplyr::do(data.grouped, pad_serie(.data, start_date, end_date, drop_last))
  dplyr::ungroup(data.grouped)
}

#' Pad serie
#'
#' @param serie a tibble in rOstluft long format containing exactly one serie
#'
#' @rdname pad
#' @export
pad_serie <- function(serie, start_date = NULL, end_date = NULL, drop_last = FALSE) {
  if (is.null(start_date)) {
    start_date <- min(serie$starttime)
  }

  if (is.null(end_date)) {
    end_date <- max(serie$starttime)
    drop_last <- FALSE
  }

  # by joining the data we insert rows with NA values for site, parameter, interval, unit, value
  # we need to fill this with the values from the supplied df
  fill.values <- dplyr::slice(serie, 1)
  fill.values <- as.list(dplyr::select(fill.values, -"starttime", -"value"))

  interval <- convert_interval(fill.values$interval)

  all.dates <- tibble::tibble(
    starttime = seq(start_date, end_date, interval)
  )

  if (isTRUE(drop_last)) {
    all.dates <- utils::head(all.dates, -1)
  }

  padded <- dplyr::full_join(all.dates, serie, by = "starttime")
  tidyr::replace_na(padded, replace = fill.values)
}
