# gleich wie bei den meteo schweiz

# bottom up ansatz

# daten aus einer datei lesen

read_ethz_10min_parameters <- function(x, site = NULL) {
  lines <- readr::read_lines(x, n_max = 60)
  from <- which(stringr::str_detect(lines, "9999"))[1]
  to <- which(lines == "5")
  header <- which(lines == "")[1]
  pars <-
    bind_cols(
      parameter_original = as.character(readr::read_table2(con, skip = header - 1, n_max = 1, col_names = FALSE))[-1],
      read_delim(x, delim = "(", skip = from, n_max = to - from - 1, col_names = FALSE)[,-1]
    ) %>%
    dplyr::rename(unit = X2)
  pars$unit <- as.character(sapply(pars$unit, function(y) str_replace_all(y, "\\(|\\)|\\ ", "")))
  pars$unit <- as.character(sapply(pars$unit, function(y) str_replace_all(y, "deg", "°")))
  pars$unit <- as.character(sapply(pars$unit, function(y) str_replace_all(y, "percent", "%")))
  return(pars)
}


### Funktion ETHZ IAC Daten einlesen
read_ethz_iac_10min <- function(x, site = NULL, encoding = "UTF-8", timezone = "GMT") {
  pars <- read_ethz_10min_parameters(x, site)
  if (is.null(site)) {site <- "ETHZ_CHN"}
  if (site == "-Hberg") {site <- "ETHZ_HBerg"}
  lines <- readr::read_lines(x, n_max = 60)
  header <- which(stringr::str_detect(lines, " time"))
  startdate <- lines[which(stringr::str_detect(lines, "time in minutes since"))]
  startdate <- substring(startdate, 23, 32)
  df <-
    readr::read_table2(x, skip = header + 1, col_names = c("time", pars$parameter_original),
                       na = c("99999.0", "9999.0"), skip_empty_rows = TRUE, locale = locale(encoding = encoding)) %>%
    mutate_all(as.numeric) %>%
    mutate(date = startdate) %>%
    gather(parameter_original, val, - date, -time) %>%
    mutate(
      time = as.POSIXct(as.Date(date) + as.difftime(time, units = "mins"), tz = timezone),
      interval = "10min",
      site_short = site,
      unit = plyr::revalue(parameter_original, setNames(pars$unit, pars$parameter_original))
    ) %>%
    dplyr::select(-date)
  return(df)
}




# read_ethz_iac <- function(file, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL) {
#   # site information is in the file, could parse from the header or provide it as argument
#   header <- readr::read_lines(file, n_max = 10L)
#
#   # invalid values include "99999.0", "9999.0", "-320" <- note sure how it is formatted
#   data <- readr::read_table(file, skip = 10L) # needs some more parameter :P
# }


get_ethz_iac_con <- function(date, site, userpw = NULL, cache = NULL) {
  con <- getURL(paste0(url$chn,"IAC-Met",site,"_", date,".dat"), userpw = userpwd)
  return(con)
}
