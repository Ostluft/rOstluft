
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
#'
#' @examples
read_ethz_iac_min10_parameters <- function(x, site = NULL) {
  lines <- readr::read_lines(x, n_max = 60)
  from <- which(stringr::str_detect(lines, "9999"))[1]
  to <- which(lines == "5")
  header <- which(lines == "")[1]
  pars <-
    bind_cols(
      parameter_original = as.character(readr::read_table2(x, skip = header - 1, n_max = 1, col_names = FALSE))[-1],
      read_delim(x, delim = "(", skip = from, n_max = to - from - 1, col_names = FALSE)[,-1]
    ) %>%
    dplyr::rename(unit = X2)
  pars$unit <- as.character(sapply(pars$unit, function(y) str_replace_all(y, "\\(|\\)|\\ ", "")))
  pars$unit <- as.character(sapply(pars$unit, function(y) str_replace_all(y, "deg", "°")))
  pars$unit <- as.character(sapply(pars$unit, function(y) str_replace_all(y, "percent", "%")))
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
#'
#' @return tibble in rOstluft long format structure
#'
#' @seealso [rOstluft::read_ethz_iac_min10_parameters]
#' @export
#'
#' @examples
read_ethz_iac_min10 <- function(x, site = NULL, encoding = "UTF-8", timezone = "Etc/GMT") {
  pars <- read_ethz_iac_min10_parameters(x, site)
  if (is.null(site)) {site <- "ETHZ_CHN"}
  if (site == "-Hberg") {site <- "ETHZ_HBerg"}
  lines <- readr::read_lines(x, n_max = 60)
  header <- which(stringr::str_detect(lines, " time"))
  startdate <- lines[which(stringr::str_detect(lines, "time in minutes since"))]
  startdate <- substring(startdate, 23, 32)
  df <-
    readr::read_table2(x, skip = header + 1, col_names = c("time", pars$parameter_original),
                       na = c("99999", "99999.0", "9999.0", "99999.00", "9999.00", "-320", "-320.0", "-320.00"),
                       skip_empty_rows = TRUE, locale = locale(encoding = encoding)) %>%
    mutate_all(as.numeric) %>%
    mutate(date = startdate) %>%
    dplyr::filter(time %% 10 == 0) %>%
    gather(parameter_original, value, - date, -time) %>%
    mutate(
      starttime = as.POSIXct(as.Date(date), tz = timezone) + as.difftime(time, units = "mins"),
      interval = "min10",
      site_short = site,
      unit = plyr::revalue(parameter_original, setNames(pars$unit, pars$parameter_original))
    ) %>%
    dplyr::select(-date, -time)
  return(df)
}



get_ethz_iac_con <- function(date, site, userpw = NULL, cache = NULL) {
  con <- getURL(paste0(url$chn,"IAC-Met",site,"_", date,".dat"), userpw = userpwd)
  return(con)
}
