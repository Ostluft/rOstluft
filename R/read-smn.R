#' Reads a Swiss Meteo Network data file
#'
#' Reads the data from a Swiss Meteo Network export and returns it as a data frame in rOstluft long format
#'
#' @param file Either a path to a file, a connection, or literal data. see [readr::read_delim()]
#' @param encoding file encoding. Default "UTF-8"
#' @param timezone time zone of date fields. Be carefull Etc/GMT + == -. Default "UTC"
#'
#' @return data frame in rOstluft long format
#'
#' @seealso [lubridate::period()] - Create or parse period objects
#' @seealso [base::timezones] - Information about time zones in R
#'
#' @export
read_meteoschweiz_smn <- function(x, timezone = "UTC", encoding = "UTF-8", time_format = "%Y%m%d%H%M") {
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
  df <- bind_rows(lapply(1:length(skip), function(y) {
    if (!is.null(units)) {
      units <- readr::read_table2(x, skip = skip[y] - 1, col_names = TRUE, locale = locale(encoding = encoding), n_max = 1, skip_empty_rows = TRUE)
      units <- setNames(as.character(units), names(units)[-c(1,2)])
      units <- sapply(units[!is.na(units)], function(z) str_replace_all(z, "\\[|\\]", ""))
    }
    readr::read_table2(x, skip = skip[y] - 1, col_names = TRUE, na = c("", "NA", "-"), locale = locale(encoding = encoding),
                       n_max =  c(skip, Inf)[y + 1] - 2 - skip[y], skip_empty_rows = TRUE) %>%
      dplyr::slice(skip2) %>%
      mutate_at(-id_cols, as.numeric) %>%
      gather(parameter_original, val, -id_cols) %>%
      mutate(
        starttime = fast_strptime(time, format = time_format, lt = FALSE, tz = timezone),
        unit = plyr::revalue(parameter_original, units)
      ) %>%
      dplyr::select(-time)
  }))
  return(df)
}





