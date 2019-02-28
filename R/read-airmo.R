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
  data <- readr::read_delim(fn, ";", col_types = readr::cols(), col_names = FALSE,
                            locale = locale, trim_ws = TRUE, progress = FALSE)
  header <- data[c(1, 2, 4, 5), -1]
  data <- utils::tail(data, -6)
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
  data <- readr::read_delim(fn, ";", col_types = readr::cols(), col_names = FALSE,
                            locale = locale, trim_ws = TRUE, progress = FALSE)
  header <- data[c(1, 5, 9, 8), -1]
  data <- utils::tail(data, -10)
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

  header_names <- lapply(header, paste, collapse = "\u00bb")
  colnames(data)[-1] <- header_names

  data[["starttime"]] <- lubridate::parse_date_time(data[["starttime"]], c("dmYHMS", "dmYHM", "dmY"), tz = tz)
  if (lubridate::is.period(time_shift)) {
    data[["starttime"]] <- data[["starttime"]] + time_shift
  }
  data_long <- tidyr::gather(data, "key", "value", -"starttime", na.rm = na.rm)
  data_long[["value"]] <- as.numeric(data_long[["value"]])
  data_long <- tidyr::separate(data_long, "key", c("site", "parameter", "interval", "unit"), sep = "\u00bb")
  dplyr::mutate_if(data_long, is.character, as.factor)
}
