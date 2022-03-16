#' Reads historic data files  of the Tecson meteo station placed at Tiefenbrunnen and Mythenquai water police station.
#'
#' @param fn file name containing historic data
#' @param site measurement site of data. Should be "ZH_Mythenquai" or "ZH_Tiefenbrunnen"
#' @param tz Output time zone. The file content is in CET. Default "Etc/GMT-1"
#' @param time_shift a lubridate period to add to the time. Default NULL
#' @param na.rm remove na (empty) values. Default TRUE
#'
#' @return data frame in rOstluft long format
#' @export
#'
#' @examples
#' input <- rOstluft.data::f("messwerte_tiefenbrunnen_2007-2019.csv")
#' data <- read_seepolizei(input, "ZH_Tiefenbrunnen")
#'
#' # normalize data with meta informations
#' meta_data <- readRDS(rOstluft.data::f("meta_seepolizei.rds"))
#'
#' # add units
#' data <- meta_apply(
#'   data = data,
#'   meta = meta_data,
#'   data_src = "parameter",
#'   data_dest = "unit",
#'   meta_key = "parameter_original",
#'   meta_val = "unit",
#'   mode = "strict"
#' )
#'
#' # rename parameters to Ostluft Conventions
#' data <- meta_apply(
#'   data = data,
#'   meta = meta_data,
#'   data_src = "parameter",
#'   data_dest = "parameter",
#'   meta_key = "parameter_original",
#'   meta_val = "parameter",
#'   mode = "strict"
#' )
#'
#' # show normalized data
#' data
read_seepolizei <- function(fn, site = c("ZH_Mythenquai", "ZH_Tiefenbrunnen"), tz = "Etc/GMT-1",  time_shift = NULL, na.rm = TRUE) {

  site <- rlang::arg_match(site, c("ZH_Mythenquai", "ZH_Tiefenbrunnen"))

  locale <- readr::locale(encoding = "UTF-8")
  cols <- readr::cols(
    timestamp_cet = readr::col_character(),
    .default = readr::col_double()
  )

  data <- readr::read_csv(fn, col_types = cols, locale = locale)

  data <- dplyr::mutate(data,
    timestamp_cet = lubridate::fast_strptime(.data$timestamp_cet, "%Y-%m-%dT%H:%M:%S",  tz = "CET", lt = FALSE)
  )

  # time information is probably endtime so we subtract 10 min.
  if (lubridate::is.period(time_shift)) {
    time_shift <- time_shift - lubridate::minutes(10)
  } else {
    time_shift <- lubridate::minutes(-10)
  }
  data <- dplyr::mutate(data, timestamp_cet = .data$timestamp_cet + time_shift)

  # convert CET to target Timezone
  if (tz != "CET") {
    data <- dplyr::mutate(data, timestamp_cet = lubridate::with_tz(.data$timestamp_cet, tzone = tz))
  }

  data <- tidyr::gather(data, key = "parameter", "value", -"timestamp_cet", factor_key = TRUE)
  data <- dplyr::mutate(data,
    interval = as.factor("min10"),
    site = as.factor(site),
    unit = as.factor(NA)
  )

  dplyr::select(data, starttime = "timestamp_cet", "site", "parameter", "interval", "unit", "value")
}


#' Helper function to ensure correct string format
#'
#' @param x date as string, POSIXct, POSIXlt or Date Object
#'
#' @return string in format "%Y-%m-%d"
#'
#' @keywords internal
convert_date_seepolizei <- function(x) {
  if (is.character(x)) {
    x <- tryCatch({
      x <- lubridate::parse_date_time(x, "%Y-%m-%d", exact = TRUE)
    }, warning = function(w) {
      stop("start and end format is yyyy-mm-dd")
    }, error = function(e) {
      stop("start and end format is yyyy-mm-dd")
    })
  } else if (!lubridate::is.timepoint(x)) {
    stop("start and end format is yyyy-mm-dd")
  }
  format(x, "%Y-%m-%d")
}

#' Returns data of the Tecson meteo station placed at Tiefenbrunnen and Mythenquai water police station.
#'
#' `get_seepolizei()` uses the [Tecdottir API](https://tecdottir.herokuapp.com/docs/) from Stefan Oderholz.
#' `read_seepolizei_json()` parses the response of the API.
#'
#'
#' @param start The start date after which the measurements should be returned. As string in format yyyy-mm-dd
#'   or as POSIXct, POSIXlt or Date
#' @param end The end date before which the measurements should be returned. As string in format yyyy-mm-dd
#'   or as POSIXct, POSIXlt or Date
#' @param site The site of which the values should be returned. Valid values are "mythenquai" and "tiefenbrunnen"
#' @param tz Output time zone. The response content is in CET. Default "Etc/GMT-1"
#' @param txt a JSON string, URL or file
#'
#' @return tibble in rolf format
#' @export
#'
#' @examples
#' get_seepolizei(as.Date("2020-03-01"), lubridate::dmy("02.03.2020"))
#'
#'
#' data <- get_seepolizei("2020-03-20", "2020-03-22", "mythenquai")
#'
#' # normalize data with meta informations
#' meta_data <- readRDS(rOstluft.data::f("meta_seepolizei.rds"))
#'
#' # rename station
#' data <- meta_apply(
#'   data = data,
#'   meta = meta_data,
#'   data_src = "site",
#'   data_dest = "site",
#'   meta_key = "site_short",
#'   meta_val = "site",
#'   mode = "strict"
#' )
#'
#' # rename parameters to Ostluft Conventions
#' data <- meta_apply(
#'   data = data,
#'   meta = meta_data,
#'   data_src = "parameter",
#'   data_dest = "parameter",
#'   meta_key = "parameter_original",
#'   meta_val = "parameter",
#'   mode = "strict"
#' )
#'
#' # show normalized data
#' data
get_seepolizei <- function(start, end, site = c("tiefenbrunnen", "mythenquai"), tz = "Etc/GMT-1") {
  site <- rlang::arg_match(site, c("tiefenbrunnen", "mythenquai"))
  start <- convert_date_seepolizei(start)
  end <- convert_date_seepolizei(end)

  url <- "https://tecdottir.herokuapp.com/measurements/${site}?startDate=${start}&endDate=${end}"
  url <- stringr::str_interp(url, list(start = start, end = end, site = site))

  # proxy 407 with jsonlite::fromJSON(url) use httr with global proxy configuration
  resp <- httr::GET(url)

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Tecdottir API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        httr::http_status(resp)$message,
        url
      ),
      call. = FALSE
    )
  }

  read_seepolizei_json(httr::content(resp, as = "text"))
}


#' @rdname get_seepolizei
#' @export
read_seepolizei_json <- function(txt, tz = "Etc/GMT-1") {
  df <- jsonlite::fromJSON(txt)
  df <- jsonlite::flatten(df$result)
  df <- tibble::as_tibble(df)

  # select value columns
  values <- dplyr::select(df,
    site = "station",
    dplyr::ends_with(".value"),
    starttime = "values.timestamp_cet.value"
  )

  value_names <- purrr::keep(rlang::names2(values), ~stringr::str_ends(., ".value"))
  parameters <- stringr::str_sub(value_names, 8, -7)
  parameters <- rlang::set_names(value_names, parameters)
  values <- dplyr::rename(values, !!!parameters)
  values <- dplyr::mutate(values, starttime = lubridate::dmy_hms(.data$starttime, tz = "CET"))
  values <- tidyr::gather(values, key = "parameter", "value", -"site", -"starttime", factor_key = TRUE)

  #unit mapping
  units <- dplyr::slice(df, 1) %>%
    dplyr::select(dplyr::ends_with(".unit"), -"values.timestamp_cet.unit")
  unit_names <- stringr::str_sub(rlang::names2(units), 8, -6)
  units <- rlang::set_names(units, unit_names) # %>% as.list

  # make rolf format
  values <- dplyr::mutate(values,
    starttime = lubridate::with_tz(starttime, tz),
    interval = as.factor("min10"),
    site = as.factor(.data$site),
    unit = dplyr::recode(.data$parameter, !!!units),
  )

  # some times value are doubles and some times characters ..
  if (is.character(values$value)) {
    values <- dplyr::mutate(values, value = readr::parse_double(.data$value))
  }

  # reorder columns
  dplyr::select(values, "starttime", "site", "parameter", "interval", "unit", "value")
}
