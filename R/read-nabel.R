read_nabel_txt <- function(file, encoding = "latin1", tz = "Etc/GMT-1", interval = NULL, time_shift = NULL) {
  header <- readr::read_lines(file, n_max = 20, locale = readr::locale(encoding = encoding))
  skip <- stringr::str_which(header, "X")
  site <- header[1]
  na_value <- stringr::str_split(header[stringr::str_which(header, "Ausgefallene Werte werden durch den Wert ")], pattern = " ")[[1]]
  na_value <- na_value[stringr::str_which(na_value, "gekennzeichnet.") - 1]
  skip2 <- stringr::str_which(header, "Bezeichnung der Datenkolonnen:")
  nrow2 <- stringr::str_which(header, "Ausgefallene Werte werden")
  parameters <- readr::read_table(file, skip = skip2, n_max = nrow2 - skip2 - 2, col_names = TRUE, col_types = readr::cols(), locale = readr::locale(encoding = encoding))
  parameters <- dplyr::mutate(parameters, Einheit = ifelse(Einheit == "-", NA, Einheit))
  units <- rlang::set_names(parameters$Einheit, parameters$Messobjekt)
  data <- readr::read_table(file, skip = skip, col_names = parameters$Messobjekt, col_types = eadr::cols(), locale = readr::locale(encoding = encoding), na = na_value)
  data <- dplyr::mutate(data, endtime = lubridate::ydm_hm(paste0(Jahr, Tag, Monat, Stunde, Minute), tz = tz))
  if (nrow(data) < 2 && is.null(interval)) {
    stop("couldn't detect interval. use argument interval")
  } else if (is.null(interval)) {
    duration <- lubridate::as.duration(data$endtime[2] - data$endtime[1])
    interval <- lubridate::time_length(duration, unit = "minutes")
    interval <- switch(as.character(interval), "10" = "min10", "30" = "min30", "60" = "h1", "1440" = "d1",
                       stop("couldn't detect interval. use argument interval"))
  } else if (!lubridate::is.period(time_shift)) {
    stop(stringr::str_c("If argument interval is used, time_shift is necessary! ",
                        "time_shift = lubridate::period(0) can be used for no shifting"))
  } else {
    interval <- interval
  }
  data <- dplyr::mutate(data, starttime = endtime - duration)
  data <- dplyr::select(data, -Jahr, -Monat, -Tag, -Stunde, -Minute, -endtime)
  data <- tidyr::gather(data, parameter, value, -starttime)
  data <- dplyr::mutate(data, 
                        site = site,
                        interval = interval,
                        unit = dplyr::recode(.data$parameter, !!!units),
                        value = ifelse(value == as.numeric(na_value), NA, value)
  )
  data <- dplyr::mutate_if(data, is.character, factor) %>% 
    dplyr::select(starttime, site, parameter, interval, unit, value)
  
  return(data)
}
