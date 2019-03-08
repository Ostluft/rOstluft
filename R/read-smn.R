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
                    "unit" = plyr::revalue(.data$parameter_original, units))
      #dplyr::select(-.data$time)
    if ("stn" %in% names(df2)) {
      df2 <- dplyr::rename(df2, "site_short" = .data$stn)
    }
    df2
  }))
  return(df)
}


#' Reads a file from the SwissMetNet of MeteoSwiss
#'
#' @description
#' [SwissMetNet](https://www.meteoswiss.admin.ch/home/measurement-and-forecasting-systems/land-based-stations/automatisches-messnetz.html),
#' the automatic monitoring network of [MeteoSwiss](https://www.meteoschweiz.admin.ch) the Federal Office for
#' Meteorology and Climatology, comprises about 160 automatic monitoring stations. These stations deliver a multitude
#' of current data on the weather and climate in Switzerland every ten minutes. The monitoring network is supplemented
#' by automatic precipitation stations.
#'
#' @param fn path to input file
#' @param tz of the output data. Default "Etc/GMT-1"
#' @param encoding encoding of the data file. Default = "UTF-8"
#' @param time_format optional time_format. Use if auto detect fails. Default NULL
#' @param interval optional interval of the data. Use if auto detect fails. Default NULL
#' @param na.rm remove na values. Default TRUE
#'
#' @return tibble in rOstluft long format structure
#'
#' @export
#'
#' @examples
#' input <- system.file("extdata", "smn.txt", package = "rOstluft.data", mustWork = TRUE)
#' read_smn(single)
#'
read_smn <- function(fn, tz = "Etc/GMT-1", encoding = "UTF-8", time_format = NULL, interval = NULL, na.rm = TRUE) {
  locale <- readr::locale(encoding = encoding)

  col_types <- readr::cols(
    stn = readr::col_character(),
    time = readr::col_character(),
    .default = readr::col_double()
  )

  # smn files tend to have atleast one line with a space at the start ... read 20 and take the first non empty ...
  header <- readr::read_lines(fn, n_max = 20)
  start_line <- purrr::detect_index(header, ~ stringr::str_trim(.) != "")
  header <- header[start_line]

  if (stringr::str_count(header, " ") > 4) {
    data <- readr::read_table2(fn, col_types = col_types, locale = locale, na = "-",
                               skip = start_line - 1, skip_empty_rows = TRUE)
  } else if (stringr::str_count(header, ";") >= 2) {
    data <- readr::read_delim(fn, ";", col_types = col_types, locale = locale, na = "-",
                              skip = start_line - 1, skip_empty_rows = TRUE)
  } else {
    stop("couldn't detect delimiter")
  }

  if (is.null(time_format)) {
    time_length <- stringr::str_length(data$time[1])                  # are we too clever with format detection?
    time_format <- stringr::str_sub("%Y%m%d%H%M", 1, time_length - 2) # Y has 4 digits but only two in format -> -2
  }

  data <- dplyr::mutate(data,
    time = lubridate::with_tz(lubridate::fast_strptime(data$time, time_format, lt = FALSE), tz)
  )

  if (nrow(data) < 2 && is.null(interval)) {
    stop("couldn't detect interval. use argument interval")
  } else if (!is.null(interval)) {
    interval <- interval
  } else {
    interval <- lubridate::as.duration(data$time[2] - data$time[1])
    interval <- lubridate::time_length(interval, unit = "minutes")
    interval <- switch(as.character(interval), "10" = "min10", "30" = "min30", "60" = "h1", "1440" = "d1",
                       stop("couldn't detect interval. use argument interval"))
  }

  data <- tidyr::gather(data, "parameter", "value", -.data$time, -.data$stn, na.rm = na.rm)
  data <- dplyr::mutate(data,
    stn = forcats::as_factor(.data$stn),
    parameter = forcats::as_factor(.data$parameter),
    interval = forcats::as_factor(interval),
    unit = factor(NA)
  )
  dplyr::select(data, starttime = "time", site = "stn", "parameter", "interval", "unit", "value")
}


#' Reads a file from the SwissMetNet of MeteoSwiss containing multiple Exports
#'
#' @description
#' One peculiarity of MeteoSwiss is to provide multiple exports concatenated in one file, starting with a line
#' containing a single space and seperated by two newlines. This function reads the whole file into the memory, splits
#' the part and applies the read function to each part. Is the file too big for the available memory, the function
#' [split_smn()] can split the file into multiple files each containing one part. On Linux the commandline tool csplit
#' is another probably faster tool.
#'
#' @seealso
#' * [read_smn()]
#' * [split_smn()]
#' * [csplit](http://man7.org/linux/man-pages/man1/csplit.1.html) - split a file into sections determined by context
#'   lines
#'
#' @param fn path to input file
#' @param as_list return a list with each part as element rather than one concatenated tibble
#' @param encoding encoding of the data file. Default = "UTF-8"
#' @param ... parameters passed to [read_smn()]
#'
#' @return tibble or list of tibbles in rOstluft long format structure
#'
#' @export
#'
#' @examples
#' fn <- system.file("extdata", "smn_multi.txt", package = "rOstluft.data", mustWork = TRUE)
#' read_smn_multiple(multi, as_list = TRUE)
#'
read_smn_multiple <- function(fn, as_list = FALSE, encoding = "UTF-8", ...) {
  data <- readr::read_file(fn, readr::locale(encoding = encoding))
  data <- stringr::str_split(data, "\r\n\r\n|\n\n")            # line end conversion happens
  data <- purrr::keep(data[[1]], ~ stringr::str_length(.) > 1) # remove empty and chunks with only a space
  data <- purrr::map(data, read_smn, encoding = encoding, ...)

  if (isFALSE(as_list)) {
    data <- bind_rows_with_factor_columns(!!!data)
  }
  data
}



#' Splits a file from the SwissMetNet of MeteoSwiss containing multiple Exports
#'
#' @param fn path to input file
#' @param out_dir Output directory for parts
#' @param suffix Added suffix to file name as sprintf format string. Gets file counter as argument.
#' @param encoding Encoding of the input file
#'
#' @return NULL
#'
#' @export

split_smn <- function(fn, out_dir = NULL, suffix = "%03d.part", encoding = "UTF-8") {
  if (is.null(out_dir)) {
    out_dir <- fs::path_dir(file)
  }

  filename <- fs::path_file(fn)

  con_in <- file(fn, open = "r", encoding = encoding)
  file_count <- 1
  line_count <- 0

  out_fn <- fs::path(out_dir, filename, ext = sprintf(suffix, file_count))
  fs::dir_create(out_dir)
  con_out <- file(out_fn, open = "w", encoding = encoding)

  message("start splitting")

  while (length(line <- readLines(con_in, n = 1, warn = FALSE)) > 0) {
    trimmed <- stringr::str_trim(line)

    # new chunk -> new file
    if ((trimmed == "") && (line_count > 0)) {
      close(con_out)
      file_count <- file_count + 1
      message(sprintf("Got %d lines in last chunk. start chunk nr %2d", line_count, file_count))
      line_count <- 0
      out_fn <- fs::path(out_dir, filename, ext = sprintf(suffix, file_count))
      con_out <- file(out_fn, open = "w", encoding = encoding)
    }

    if (trimmed != "") {
      writeLines(line, con_out)
      line_count <- line_count + 1
    }
  }

  close(con_in)
  close(con_out)
  if (line_count > 0) {
    message(sprintf("Got %d lines in last chunk. Finished file with %2d chunks", line_count, file_count))
  } else {
    fs::file_delete(out_fn)
    message(sprintf("Last chunk is empty. Finished file with %2d chunks", file_count - 1))
  }

  invisible(NULL)
}




