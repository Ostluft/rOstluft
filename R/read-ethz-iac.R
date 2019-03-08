
#' Derive metadata (parameters, units etc) from an IAC/ETHZ meteo station file
#'
#' @description Reads metadata from meteo station measurement files (10 minute time resolution) by the institute for atmosphere and climety at ETH Zürich.
#' Currently, two stations in Zürich are under long-term operation: CHN-Gebäude & Hönggerberg (see http://www.iac.ethz.ch/the-institute/weather-stations.html)

#' @param x Either a path to a file, a connection, or literal data. see [readr::read_delim()]
#' @param site Character string specifying the site of the meteo station. Needs to be exactly like in file name,
#' e.g. "-Hberg" for files from Hönggerberg station. For site 'CHN Gebäude' the default of site is NULL (due to respective file naming)
#'
#' @return tibble in rOstluft long format structure
#' @export
read_ethz_iac_min10_parameters <- function(x, site = NULL) {
  lines <- readr::read_lines(x, n_max = 60)
  from <- which(stringr::str_detect(lines, "9999"))[1]
  to <- which(lines == "5")
  header <- which(lines == "")[1]
  pars <-
    dplyr::bind_cols(
      "parameter_original" = as.character(readr::read_table2(x, skip = header - 1, n_max = 1, col_names = FALSE))[-1],
      readr::read_delim(x, delim = "(", skip = from, n_max = to - from - 1, col_names = FALSE, col_types = readr::cols())[,-1]
    ) %>%
    dplyr::rename("unit" = .data$X2)
  pars$unit <- as.character(sapply(pars$unit, function(y) stringr::str_replace_all(y, "\\(|\\)|\\ ", "")))
  pars$unit <- as.character(sapply(pars$unit, function(y) stringr::str_replace_all(y, "deg", "\u00b0")))
  pars$unit <- as.character(sapply(pars$unit, function(y) stringr::str_replace_all(y, "percent", "%")))
  return(pars)
}


#' Read an IAC/ETHZ meteo station file
#'
#' @description Reads files from meteo station measurements (10 minute time resolution) by the institute for atmosphere and climety at ETH Zürich.
#' Currently, two stations in Zürich are under long-term operation: CHN-Gebäude & Hönggerberg (see http://www.iac.ethz.ch/the-institute/weather-stations.html)
#'
#' @param x Either a path to a file, a connection, or literal data. see [readr::read_delim()]
#' @param site Character string specifying the site of the meteo station. Needs to be exactly like in file name,
#' e.g. "-Hberg" for files from Hönggerberg station. For site 'CHN Gebäude' the default of site is NULL (due to respective file naming)
#' @param encoding encoding of the data file
#' @param timezone of the containing data
#'
#' @return tibble in rOstluft long format structure
#'
#' @seealso [rOstluft::read_ethz_iac_min10_parameters]
#' @export
read_ethz_iac_min10 <- function(x, site = NULL, encoding = "UTF-8", timezone = "Etc/GMT") {
  pars <- read_ethz_iac_min10_parameters(x, site)
  if (is.null(site)) {site <- "ETHZ_CHN"}
  if (site == "-Hberg") {site <- "ETHZ_HBerg"}
  lines <- readr::read_lines(x, n_max = 60)
  header <- which(stringr::str_detect(lines, " time"))
  startdate <- lines[which(stringr::str_detect(lines, "time in minutes since"))]
  startdate <- substring(startdate, 23, 32)
  df <-
    readr::read_table2(x, skip = header + 1, col_names = c("time", pars$parameter_original), col_types = readr::cols(),
                       na = c("99999", "99999.0", "9999.0", "99999.00", "9999.00", "-320", "-320.0", "-320.00"),
                       skip_empty_rows = TRUE, locale = readr::locale(encoding = encoding)) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate("date" = startdate) %>%
    dplyr::filter(.data$time %% 10 == 0) %>%
    tidyr::gather("parameter_original", "value", - .data$date, -.data$time) %>%
    dplyr::mutate(
      "starttime" = as.POSIXct(as.Date(.data$date), tz = timezone) + as.difftime(.data$time, units = "mins"),
      "interval" = forcats::as_factor("min10"),
      "site_short" = site,
      "unit" = plyr::revalue(.data$parameter_original, rlang::set_names(pars$unit, pars$parameter_original))
    ) %>%
    dplyr::select(-.data$date, -.data$time)

  return(df)
}


#' Read an IAC/ETHZ meteo station file
#'
#' Reads files from meteo station measurements (10 minute time resolution) by the institute for atmosphere and climety
#' at ETH Zürich. Currently, two stations in Zürich are under long-term operation: CHN-Gebäude & Hönggerberg (see
#' [http://www.iac.ethz.ch/the-institute/weather-stations.html](Weather Stations)).
#' The files are daily files, encoded in utf-8. Timezone is GMT.
#'
#' @param x Either a path to a file, a connection, or literal data. see [readr::read_delim()]
#' @param tz of the output data. Default "Etc/GMT-1"
#' @param site Character string specifying the site of the meteo station. Usally the site will be autodetected from the
#'   header. But if autodection fails or an overwrite is needed set site. Default NULL
#' @param na.rm remove na values. Default TRUE
#' @param encoding encoding of the data file. Default = "UTF-8"
#'
#' @return tibble in rOstluft long format structure
#'
#' @export
read_ethz_iac <- function(x, tz = "Etc/GMT-1", site = NULL, na.rm = TRUE, encoding = "UTF-8") {
  # ethz files are quite small just read it into the memory. user fault if he supplies one big file :p
  locale <-  readr::locale(encoding = encoding)
  txt <- readr::read_file(x, locale = locale)

  # split header from parameter, units, data
  txt <- stringr::str_split_fixed(txt, "\r\n\r\n|\n\n", n = 2)  # orig files have crlf endings, but shit happens
  header <- txt[1, 1]

  # get line with startdate
  startdate <- stringr::str_extract(header, "time in minutes since.*")
  startdate <- stringr::str_sub(startdate, 23, 32) # do we need a more sophisticated method? we could get the tz too?
  startdate <- lubridate::ymd(startdate, tz = "GMT")

  # get line with location
  site_line <- stringr::str_extract(header, "location:.*")
  if (!is.null(site) && is.character(site)) {
    site <- site
  } else if (stringr::str_detect(site_line, "roof of building CHN")) {
    site <- "ETHZ_CHN-Gebäude"
  } else if (stringr::str_detect(site_line, "Messfeld HPS G41")) { # <--- better string needed
    site <- "ETHZ_Hönggerberg"
  } else {
    stop("Unkown site! Use argument site as character")
  }

  # parameter + units are in the first two lines
  pars <- readr::read_lines(txt[1, 2], n_max = 2, locale = locale)
  parameters <- stringr::str_split(pars[1], "\\s+", simplify = TRUE)[1, -1]

  # replace brackets with space -> percent)(log10lux is a problem without the spaces and the reason we don't use
  # read_table ...
  units <- stringr::str_replace_all(pars[2], "\\(|\\)", " ")
  units <- stringr::str_replace_all(units, "deg ", "\u00b0")  # we replace "deg " = "°", "deg C" => "°C" should be safe
  units <- stringr::str_replace_all(units, "percent", "%")
  units <- stringr::str_split(units, "\\s+")[[1]]
  units <- units[2:( length(units) - 1)]  # replacing brackets with spaces adds one at the end too

  # skip 2 lines, the rest is numeric data
  data <- readr::read_table(txt[1, 2], col_names = FALSE, skip = 2, skip_empty_rows = TRUE, locale = locale,
                            col_types = readr::cols(.default = readr::col_number()),
                            na = c("99999", "99999.0", "9999.0", "99999.00", "9999.00", "-320", "-320.0", "-320.00"))

  # remove status columns
  no_status_cols <- !stringr::str_detect(units, "status")
  parameters <- parameters[no_status_cols]
  units <- units[no_status_cols]
  data <- data[ , no_status_cols]

  # use parameters as name
  data <- rlang::set_names(data, parameters)

  # filter NA time and not on 10min interval, and remove duplicated timestamps, calculate starttime
  data <- dplyr::filter(data, .data$time %% 10 == 0)
  data <- dplyr::distinct(data, .data$time, .keep_all = TRUE)
  data <- dplyr::mutate(data,
    starttime = lubridate::with_tz(startdate + lubridate::make_difftime(minute = .data$time), tz = tz)
  )

  # lookup for parameters
  units <- rlang::set_names(units, parameters)

  # reform data. data is now time, starttime + one col per parameter
  data <- tidyr::gather(data, "parameter", "value", -.data$time, -.data$starttime, na.rm = na.rm)
  data <- dplyr::mutate(data,
    site = as.factor(site),
    interval = as.factor("min10"),
    unit = forcats::as_factor(units[.data$parameter]), # no recode possible. multiple parameters have the same unit
    parameter = forcats::as_factor(.data$parameter)    # parameter after unit or lookup will use factor value not level
  )

  dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
}
