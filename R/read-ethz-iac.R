#' Read an IAC/ETHZ meteo station file
#'
#' Reads files from meteo station measurements (10 minute time resolution) by the institute for atmosphere and climety
#' at ETH Zürich. Currently, two stations in Zürich are under long-term operation: CHN-Gebäude & Hönggerberg (see
#' [Weather Stations](http://www.iac.ethz.ch/the-institute/weather-stations.html)).
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
    site <- "ETHZ_CHN-Geb\u00e4ude"
  } else if (stringr::str_detect(site_line, "Messfeld HPS G41")) {
    site <- "ETHZ_H\u00f6nggerberg"
  } else {
    stop("Unkown site! Use argument site as character")
  }

  # parameter + units are in the first two lines
  pars <- readr::read_lines(I(txt[1, 2]), n_max = 2, locale = locale)
  parameters <- stringr::str_split(pars[1], "\\s+", simplify = TRUE)[1, -1]

  # replace brackets with space -> percent)(log10lux is a problem without the spaces and the reason we don't use
  # read_table ...
  units <- stringr::str_replace_all(pars[2], "\\(|\\)", " ")
  units <- stringr::str_replace_all(units, "deg ", "\u00b0")  # replace "deg " = "°", "deg C" => "°C" should be safe
  units <- stringr::str_replace_all(units, "percent", "%")
  units <- stringr::str_split(units, "\\s+")[[1]]
  units <- units[2:( length(units) - 1)]  # replacing brackets with spaces adds one at the end too

  # skip 2 lines, the rest is numeric data
  data <- readr::read_table(I(txt[1, 2]), col_names = FALSE, skip = 2, skip_empty_rows = TRUE, locale = locale,
                            col_types = readr::cols(.default = readr::col_number()),
                            na = c("99999", "99999.0", "9999.0", "99999.00", "9999.00", "-320", "-320.0", "-320.00"))

  # remove status columns
  no_status_cols <- !stringr::str_detect(units, "status")
  parameters <- parameters[no_status_cols]
  units <- units[no_status_cols]
  data <- data[ , no_status_cols] # nolint

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
  data <- tidyr::gather(data, "parameter", "value", -"time", -"starttime", na.rm = na.rm)
  data <- dplyr::mutate(data,
    site = as.factor(site),
    interval = as.factor("min10"),
    unit = forcats::as_factor(units[.data$parameter]), # no recode possible. multiple parameters have the same unit
    parameter = forcats::as_factor(.data$parameter)    # parameter after unit or lookup will use factor value not level
  )

  dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
}
