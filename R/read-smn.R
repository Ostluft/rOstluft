#' Reads a file from the SwissMetNet of MeteoSwiss
#'
#' @description
#' [SwissMetNet](https://www.meteoswiss.admin.ch/home/measurement-and-forecasting-systems/land-based-stations/automatisches-messnetz.html),
#' the automatic monitoring network of [MeteoSwiss](https://www.meteoschweiz.admin.ch) the Federal Office for
#' Meteorology and Climatology, comprises about 160 automatic monitoring stations. These stations deliver a multitude
#' of current data on the weather and climate in Switzerland every ten minutes. The monitoring network is supplemented
#' by automatic precipitation stations.
#'
#' This Function autodetects the delimeter and tries to find the interval. In Addition the time information in the
#' files is utc and end time. The time is converted to start time and the time zone defined trough the argument tz.
#' The argument time_shift provides a way to manuelly shift the time series. In this case *no* automatically shifting
#' is applied. The provided values is directly added to information in the file.
#'
#' @param fn path to input file
#' @param tz of the output data. Default "Etc/GMT-1"
#' @param encoding encoding of the data file. Default = "UTF-8"
#' @param time_shift a lubridate period to add to the time. Default NULL
#' @param time_format optional time_format. Use if auto detect fails. Default NULL
#' @param interval optional interval of the data. Use if auto detect fails. Default NULL. If used it is necessary to
#'   define time_shift manuelly. lubridate::period(0) can be used for no shifting
#' @param na.rm remove na values. Default TRUE
#'
#' @return tibble in rOstluft long format structure
#'
#' @export
#'
#' @examples
#' input <- system.file("extdata", "smn.txt", package = "rOstluft.data", mustWork = TRUE)
#' read_smn(input)
#'
read_smn <- function(fn, tz = "Etc/GMT-1", encoding = "UTF-8", time_shift = NULL, time_format = NULL,
                     interval = NULL, na.rm = TRUE) {
  locale <- readr::locale(encoding = encoding)

  col_types <- readr::cols(
    X1 = readr::col_character(),
    X2 = readr::col_character(),
    .default = readr::col_number()
  )

  # smn files tend to have different headers, but there are always the col name starting mit stn
  header <- readr::read_lines(fn, n_max = 20)
  start_line <- purrr::detect_index(header, ~ any(stringr::str_detect(., c("stn", "Sta."))))

  if (start_line == 0) {
    stop("couldn't find a line starting with stn")
  }

  col_names <- header[start_line]

  # has the file an unit line?
  if (stringr::str_count(header[start_line + 1], "\\[") > 0) {
    skip <- start_line + 1
    units <- header[start_line + 1]
    units <- stringr::str_replace_all(units, "\\[|\\]", "")
  } else {
    skip <- start_line
    units <- NULL
  }

  if (stringr::str_count(col_names, " ") > 4) {
    col_names <- stringr::str_split(col_names, "\\s+")[[1]]
    if (!is.null(units)) {
      units <- stringr::str_split(units, "\\s+")[[1]]
    }
    data <- readr::read_table2(fn, FALSE, col_types, locale, "-", skip, skip_empty_rows = TRUE)
  } else if (stringr::str_count(col_names, ";") >= 2) {
    col_names <- stringr::str_split(col_names, ";")[[1]]
    if (!is.null(units)) {
      units <- stringr::str_split(units, ";")[[1]]
    }
    data <- readr::read_delim(fn, ";", col_types = col_types, col_names = FALSE, locale = locale, na = "-",
                              skip = skip, skip_empty_rows = TRUE)
  } else {
    stop("couldn't detect delimiter")
  }

  # check for empty columnes
  empty_cols <- which(col_names == "")
  if(length(empty_cols) != 0) {
    col_names<- col_names[-empty_cols]
    data <- dplyr::select(data, -!!empty_cols)
  }

  # normalize col_names
  col_names[stringr::str_detect(col_names, "Sta.")] <- "stn"
  col_names[stringr::str_detect(col_names, "Date")] <- "time"


  data <- rlang::set_names(data, col_names)

  if (is.null(time_format)) {
    time_length <- stringr::str_length(data$time[1])                  # are we too clever with format detection?
    time_format <- stringr::str_sub("%Y%m%d%H%M", 1, time_length - 2) # Y has 4 digits but only two in format -> -2
  }

  data <- dplyr::mutate(data,
                        time = lubridate::with_tz(lubridate::fast_strptime(data$time, time_format, lt = FALSE), tz)
  )

  if (nrow(data) < 2 && is.null(interval)) {
    stop("couldn't detect interval. use argument interval")
  } else if (is.null(interval)) {
    duration <- lubridate::as.duration(data$time[2] - data$time[1])
    interval <- lubridate::time_length(duration, unit = "minutes")
    interval <- switch(as.character(interval), "10" = "min10", "30" = "min30", "60" = "h1", "1440" = "d1",
                       stop("couldn't detect interval. use argument interval"))
  } else if (!lubridate::is.period(time_shift)) {
    stop(stringr::str_c("If argument interval is used, time_shift is necessary! ",
                        "time_shift = lubridate::period(0) can be used for no shifting"))
  } else {
    interval <- interval
  }

  if (lubridate::is.period(time_shift)) {
    data <- dplyr::mutate(data, time = .data$time + time_shift)
  } else if (is.null(time_shift)) {
    data <- dplyr::mutate(data, time = .data$time - duration)
  } else {
    stop("time_shift has to be a lubridate::period or NULL")
  }

  data <- tidyr::gather(data, "parameter", "value", -.data$time, -.data$stn, na.rm = na.rm)
  data <- dplyr::mutate(data,
    stn = forcats::as_factor(.data$stn),
    parameter = forcats::as_factor(.data$parameter),
    interval = forcats::as_factor(interval),
    unit = factor(NA)
  )

  if (!is.null(units)) {
    parameters <- utils::tail(col_names, -2)  # the first two col_names are stn and time not parameters
    units <- utils::tail(units, length(parameters)) # just take the number of parameters form the end
    units <- rlang::set_names(units, parameters)
    data <- dplyr::mutate(data, unit = dplyr::recode_factor(.data$parameter, !!!units))
  }

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
#' read_smn_multiple(fn, as_list = TRUE)
#'
read_smn_multiple <- function(fn, as_list = FALSE, encoding = "UTF-8", ...) {
  data <- readr::read_file(fn, readr::locale(encoding = encoding))
  data <- stringr::str_split(data, "\r\n\r\n|\n\n")            # line end conversion happens
  data <- purrr::keep(data[[1]], ~ any(stringr::str_detect(., c("stn", "Sta.")))) # remove empty and chunks with only a space
  data <- purrr::map(data, read_smn, encoding = encoding, ...)

  if (isFALSE(as_list)) {
    data <- dplyr::bind_rows(!!!data)
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

  #message("start splitting")

  # read first line. drop it if whitespace, else keep it.
  # without an additional chunk is genereated with files starting space\n\n
  line <- readLines(con_in, n = 1, warn = FALSE)
  if (stringr::str_trim(line) != "") {
    writeLines(line, con_out)
  }

  while (length(line <- readLines(con_in, n = 1, warn = FALSE)) > 0) {

    # new chunk -> new file
    if ((line == "") && (line_count > 0)) {
      close(con_out)
      file_count <- file_count + 1
      message(sprintf("Got %d lines in last chunk. start chunk nr %2d", line_count, file_count))
      line_count <- 0
      out_fn <- fs::path(out_dir, filename, ext = sprintf(suffix, file_count))
      con_out <- file(out_fn, open = "w", encoding = encoding)
    }

    if (line != "") {
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
