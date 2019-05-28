#' Read a file from Nabel
#'
#' @description
#' Reads Exports from [National Air Pollution Monitoring Network (NABEL)](https://www.bafu.admin.ch/bafu/en/home/topics/air/state/data/national-air-pollution-monitoring-network--nabel-.html)
#' The National Air Pollution Monitoring Network (NABEL) measures air pollution at 16 locations in Switzerland.
#' The stations are distributed throughout the country and monitor pollution at typical locations (e.g.
#' city-centre streets, residential areas, rural stations). The monitoring network has commenced operations in stages
#' since 1979 and is operated by the Federal office for the environment and Empa
#'
#' This function reads the parameter and unit information from the header. The interval is auto detected if possible.
#' In Addition the time information are in end time. The time is converted to start time and the time zone defined
#' trough the argument tz.
#' The argument time_shift provides a way to manuelly shift the time series. In this case *no* automatically shifting
#' is applied. The provided values is directly added to information in the file.
#'
#' @param fn  path to input file
#' @param encoding encoding of the data file. Default = "latin1"
#' @param tz of the data. Default "Etc/GMT-1"
#' @param interval optional interval of the data. Use if auto detect fails. Default NULL. If used it is necessary to
#'   define time_shift manuelly. lubridate::period(0) can be used for no shifting
#' @param time_shift a lubridate period to add to the time. Default NULL
#' @param na.rm remove na values. Default TRUE
#'
#' @return tibble in rolf format
#' @export
read_nabel_txt <- function(fn, encoding = "latin1", tz = "Etc/GMT-1", interval = NULL,
                           time_shift = NULL, na.rm = TRUE) {

  locale <- readr::locale(encoding = encoding)
  col_types <- readr::cols()

  # read header @ the start of the file
  header <- readr::read_lines(fn, n_max = 40, locale = locale)
  skip <- stringr::str_which(header, "X")
  site <- header[1]

  # find value for NA
  na_value <- header[stringr::str_which(header, "Ausgefallene Werte werden durch den Wert ")]
  na_value <- stringr::str_split(na_value, pattern = " ")[[1]]
  na_value <- na_value[stringr::str_which(na_value, "gekennzeichnet.") - 1]

  # find and parse parameter table
  skip2 <- stringr::str_which(header, "Bezeichnung der Datenkolonnen:")
  nrow2 <- stringr::str_which(header, "Ausgefallene Werte werden")
  parameters <- readr::read_table(fn, skip = skip2, n_max = nrow2 - skip2 - 2, col_names = TRUE,
                                  col_types = col_types, locale = locale)
  parameters <- dplyr::mutate(parameters, Einheit = ifelse(.data$Einheit == "-", NA, .data$Einheit))
  units <- rlang::set_names(parameters$Einheit, parameters$Messobjekt)

  # finally read the data, use read_table2, read_table has problems with right aligned data
  data <- readr::read_table2(fn, skip = skip, col_names = parameters$Messobjekt,
                            col_types = col_types, locale = locale, na = na_value)

  # unite the date parts and convert it to a PosixCT
  data <- tidyr::unite(data, col = "starttime", sep = "",
                       .data$Jahr, .data$Tag, .data$Monat, .data$Stunde, .data$Minute)
  data <- dplyr::mutate(data, starttime = lubridate::ydm_hm(.data$starttime, tz = tz))

  # auto detect interval
  if (nrow(data) < 2 && is.null(interval)) {
    stop("couldn't detect interval. use argument interval")
  } else if (is.null(interval)) {
    duration <- lubridate::as.duration(data$starttime[2] - data$starttime[1])
    interval <- lubridate::time_length(duration, unit = "minutes")
    interval <- switch(as.character(interval), "10" = "min10", "30" = "min30", "60" = "h1", "1440" = "d1",
                       stop("couldn't detect interval. use argument interval"))
  } else if (!lubridate::is.period(time_shift)) {
    stop(stringr::str_c("If argument interval is used, time_shift is necessary! ",
                        "time_shift = lubridate::period(0) can be used for no shifting"))
  } else {
    interval <- interval
  }

  # the time information is in endtime -> no user defined time shift, subtract duration of interval
  if (lubridate::is.period(time_shift)) {
    data <- dplyr::mutate(data, starttime = .data$starttime + time_shift)
  } else if (is.null(time_shift)) {
    data <- dplyr::mutate(data, starttime = .data$starttime - duration)
  } else {
    stop("time_shift has to be a lubridate::period or NULL")
  }

  # wrangle data
  data <- tidyr::gather(data, "parameter", "value", -.data$starttime)
  data <- dplyr::mutate(data,
      site = factor(site),
      interval = factor(interval),
      parameter = forcats::as_factor(.data$parameter),
      unit = dplyr::recode(.data$parameter, !!!units),
      value = ifelse(.data$value == as.numeric(na_value), NA, .data$value)
  )

  if (isTRUE(na.rm)) {
    data <- dplyr::filter(data, !is.na(.data$value))
  }

  dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
}
