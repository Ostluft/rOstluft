#' Reads an AIRMO Export in DAT Format
#'
#' Reads the data from a AIRMO DAT file and returns it as a data frame in rOstluft long format
#'
#' @param fn DAT file name, can be relative to current directory or absolut
#' @param encoding DAT file encoding. Default "latin1"
#' @param tz time zone of date field. Be carefull Etc/GMT + == -. Default "Etc/GMT-1"
#' @param time_shift a lubridate period to add to the time. Default NULL
#' @param na.rm remove na (empty) values. Default TRUE
#'
#' @return data frame in rOstluft long format
#'
#' @seealso [lubridate::period()] - Create or parse period objects
#' @seealso [base::timezones] - Information about time zones in R
#'
#' @export
#' @md
read_airmo_dat <- function(fn, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL, na.rm = TRUE) {
  locale <- readr::locale(encoding = encoding)

  header_cols <- readr::cols(
    X1 = readr::col_skip(),
    .default = readr::col_character()
  )

  data_cols <- readr::cols(
    X1 = readr::col_character(),
    .default = readr::col_double()
  )

  header <- readr::read_delim(fn, ";", n_max = 6,  col_types = header_cols, col_names = FALSE,  locale = locale,
                              trim_ws = TRUE, progress = FALSE)

  data <- readr::read_delim(fn, ";", skip = 6, col_types = data_cols, col_names = FALSE,
                            locale = locale, trim_ws = TRUE, progress = FALSE)

  header <- header[c(1, 2, 4, 5), ]
  airmo_wide_to_long(header, data, tz, time_shift, na.rm)
}


#' Reads an AIRMO Export in CSV Format
#'
#' Reads the data from a AIRMO CSV file and returns it as a data frame in rOstluft long format
#'
#' @param fn CSV file name, can be relative to current directory or absolut
#' @param encoding CSV file encoding. Default "latin1"
#' @param tz time zone of date field. Be carefull Etc/GMT + == -. Default "Etc/GMT-1"
#' @param time_shift a lubridate period to add to the time. Default NULL
#' @param na.rm remove na (empty) values. Default TRUE
#'
#' @return data frame in rOstluft long format
#'
#' @seealso [lubridate::period()] - Create or parse period objects
#' @seealso [base::timezones] - Information about time zones in R
#'
#' @export
read_airmo_csv <- function(fn, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL, na.rm = TRUE) {
  locale <- readr::locale(encoding = encoding)

  header_cols <- readr::cols(
    X1 = readr::col_skip(),
    .default = readr::col_character()
  )

  data_cols <- readr::cols(
    X1 = readr::col_character(),
    .default = readr::col_double()
  )

  header <- readr::read_delim(fn, ";", n_max = 10,  col_types = header_cols, col_names = FALSE,  locale = locale,
                              trim_ws = TRUE, progress = FALSE)

  data <- readr::read_delim(fn, ";", skip = 10, col_types = data_cols, col_names = FALSE,
                            locale = locale, trim_ws = TRUE, progress = FALSE)

  header <- header[c(1, 5, 9, 8), ]
  airmo_wide_to_long(header, data, tz, time_shift, na.rm)
}

#' Reads an AIRMO Export in CSV Webexport Format
#'
#' Reads the data from a AIRMO Webexport CSV file and returns it as a data frame in rOstluft long format
#'
#' @param fn CSV file name, can be relative to current directory or absolut
#' @param encoding CSV file encoding. Default "latin1"
#' @param tz time zone of date field. Be carefull Etc/GMT + == -. Default "Etc/GMT-1"
#' @param time_shift a lubridate period to add to the time. Default NULL
#' @param na.rm remove na (empty) values. Default TRUE
#'
#' @return data frame in rOstluft long format
#'
#' @seealso [lubridate::period()] - Create or parse period objects
#' @seealso [base::timezones] - Information about time zones in R
#'
#' @export
#' @examples
#' input <- rOstluft.data::f("ol_nox_covid19_2020.csv")
#' res <- read_airmo_webexport(input, na.rm = FALSE)
read_airmo_webexport <- function(fn, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL, na.rm = TRUE) {
  locale <- readr::locale(encoding = encoding)

  header_cols <- readr::cols(
    X1 = readr::col_skip(),
    .default = readr::col_character()
  )

  data_cols <- readr::cols(
    X1 = readr::col_character(),
    .default = readr::col_double()
  )

  header <- readr::read_delim(fn, ";", n_max = 7,  col_types = header_cols, col_names = FALSE,  locale = locale,
                              trim_ws = TRUE, progress = FALSE)

  data <- readr::read_delim(fn, ";", skip = 7, col_types = data_cols, col_names = FALSE,
                            locale = locale, trim_ws = TRUE, progress = FALSE)

  header <- header[c(1, 2, 4, 5), ]
  airmo_wide_to_long(header, data, tz, time_shift, na.rm)
}


#' Converts the wide format from the files to the long format
#'
#' @param header data frame containing airmo_kurzname, parameter, zeitfenster and einheit
#' @param data data frame only containing the data from the file
#' @param tz time zone of date field. Be carefull Etc/GMT + == -. Default "Etc/GMT-1"
#' @param time_shift a lubridate period to add to the time. Default NULL
#' @param na.rm remove na (empty) values. Default TRUE
#'
#' @return data frame in rOstluft long format
#'
#' @seealso [lubridate::period()] - Create or parse period objects
#' @seealso [base::timezones] - Information about time zones in R
#'
#' @keywords internal
airmo_wide_to_long <- function(header, data, tz = "Etc/GMT-1", time_shift = NULL, na.rm = TRUE) {
  colnames(data)[1] <- "starttime"
  col_ids <- rlang::names2(data)[-1]

  sites <- c(header[1, ], recursive = TRUE)
  sites <- rlang::set_names(sites, col_ids)
  parameters <- c(header[2, ], recursive = TRUE)
  parameters <- rlang::set_names(parameters, col_ids)
  intervals <- c(header[3, ], recursive = TRUE)
  intervals <- rlang::set_names(intervals, col_ids)
  units <- c(header[4, ], recursive = TRUE)
  units <- rlang::set_names(units, col_ids)

  data <- dplyr::mutate(data,
                        starttime = lubridate::parse_date_time(.data$starttime, c("dmYHMS", "dmYHM", "dmY"), tz = tz)
  )

  if (lubridate::is.period(time_shift)) {
    data <- dplyr::mutate(data, starttime = .data$starttime + time_shift)
  }

  data <- tidyr::gather(data, "id", "value", -"starttime", na.rm = na.rm, factor_key = TRUE)

  data <- dplyr::mutate(data,
                        site = dplyr::recode(.data$id, !!!sites),
                        parameter = dplyr::recode(.data$id, !!!parameters),
                        interval = dplyr::recode(.data$id, !!!intervals),
                        unit = dplyr::recode(.data$id, !!!units)
  )

  dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
}
