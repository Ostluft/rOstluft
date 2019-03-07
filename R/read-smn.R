#' Read a MeteoSchweiz SwissMetNet export file
#'
#' @description Read a MeteoSchweiz SwissMetNet export text file (such as '') and restructure the data into long format
#'
#' @param x Either a path to a file, a connection, or literal data. see [readr::read_delim()]
#' @param encoding file encoding. Default: "UTF-8"
#' @param timezone time zone of date fields. Be carefull Etc/GMT '+' actually signifies '-' and vice versa, e.g.
#'   'Etc/GMT-1' = UTC + 1. Default: "Etc/GMT"
#' @param time_format of data in file, different for 10min and 1H
#'
#' @return tibble in rOstluft long format structure
#'
#' @seealso [base::timezones] - Information about time zones in R
#' @seealso [base::strptime] - Information about time format strings in R
#'
#' @export
read_meteoschweiz_smn <- function(x, timezone = "Etc/GMT", encoding = "UTF-8", time_format = "%Y%m%d%H%M") {
  lines <- readr::read_lines(x)
  lines <- lines[lines != ""]
  skip <- which(stringr::str_detect(lines, "time"))
  skip2 <- which(stringr::str_detect(lines, "\\["))
  if (length(skip2) == 0) {
    units <- NULL
    skip2 <- 1:length(lines)
    id_cols <- 1
  } else {
    units <- NA
    skip2 <- -1
    id_cols <- 1:2
  }
  df <- dplyr::bind_rows(lapply(1:length(skip), function(y) {
    if (!is.null(units)) {
      units <- readr::read_table2(x, skip = skip[y] - 1, col_types = readr::cols(),
                                  col_names = TRUE, locale = readr::locale(encoding = encoding),
                                  n_max = 1, skip_empty_rows = TRUE)
      units <- rlang::set_names(as.character(units)[-((length(units)-1):length(units))], names(units)[-c(1,2)])
      units <- sapply(units[!is.na(units)], function(z) stringr::str_replace_all(z,
                                                                                 "\\[|\\]", ""))
    }
    df2 <- readr::read_table2(x, skip = skip[y] - 1, col_types = readr::cols(), col_names = TRUE,
                              na = c("", "NA", "-"), locale = readr::locale(encoding = encoding),
                              n_max = c(skip, Inf)[y + 1] - 2 - skip[y], skip_empty_rows = TRUE) %>%
      dplyr::slice(skip2) %>%
      dplyr::mutate_at(-id_cols, as.numeric) %>%
      tidyr::gather("parameter_original", "value", -id_cols) %>%
      dplyr::mutate("starttime" = lubridate::fast_strptime(as.character(.data$time), format = time_format, lt = FALSE, tz = timezone),
                    "unit" = plyr::revalue(.data$parameter_original, units)) %>%
      dplyr::select(-.data$time)
    if ("stn" %in% names(df2)) {
      df2 <- dplyr::rename(df2, "site_short" = .data$stn)
    }
    df2
  }))
  return(df)
}




