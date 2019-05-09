#' Converts data from rolf format to an openair compatible Format
#'
#' [openair::openair] provides Tools for the analysis of air pollution data. rOstluft focus lies in providing data
#' from different sources to the user. The openair data format is described in [openair::mydata]. Basically it is a
#' wide format with a field "date" as POSIXct or Date and the parameters as columns. Another convention are the fields
#' "ws" for wind speed and "wd" for wind direction. Although usually the functions allows passing of the names from the
#' wind fields with the arguments wd and ws. One disadvantage of the wide format is the loss of unit information.
#' Normally all parameters are in expressed in mass terms. See [openair::importKCL()] and [openair::importAURN()].
#' This functions saves the parameter <> unit information in the attribute "units". unfortunately attributes tend to be
#' lost by data wrangling functions. But an avid user might save the information.
#'
#' Some functions, for example [openair::timeAverage], have the argument "type". This argument allows to specifiy
#' columns as grouping columns. However, if type is not supplied grouping variables (character or factor) will be
#' dropped. Most other functions need a mandatory assignment of the argument pollutant.
#'
#' @param data in rolf format
#' @param as_list optional, if TRUE returns a list a tibble for each parameter and keeps the unit information. Handy
#'   for applying functions with purrr.
#' @param interval optional filter for interval
#' @param keep_ppb usually volume concentrations are not used for analyses and are dropped with the exception of NOx.
#'   When FALSE drops parameters with ppb/ppm units except NOx. Default FALSE
#' @param keep_interval keep the "interval" column. Default FALSE
#' @param ws renames this parameter to "ws". Default "WVv". Set ws = NULL to disable renaming
#' @param wd renames this parameter to "wd". Default "WD". Set wd = NULL to disable renaming
#'
#' @return tibble in openair compatible format
#' @export
#'
#' @examples
#' h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv",
#'                    package = "rOstluft.data", mustWork = TRUE)
#'
#' airmo_h1 <- read_airmo_csv(h1)
#' rolf_to_openair(airmo_h1)
#'
rolf_to_openair <- function(data, as_list = FALSE, interval = NULL, keep_ppb = FALSE, keep_interval = FALSE,
                            ws = "WVv", wd = "WD") {
  if (!is.null(interval)) {
    data <- dplyr::filter(data, .data$interval == !!interval)
  }

  if (dplyr::n_distinct(data$interval) > 1) {
    stop("More than one interval. Please use argument interval to select one.")
  }

  if (isFALSE(keep_ppb)) {
    data <-  dplyr::filter(data, stringr::str_starts(.data$parameter, "NOx") |
                             !(.data$unit == "ppb" | .data$unit == "ppm"))
  }

  if (!is.null(ws) && ws %in% levels(data$parameter)) {
    data$parameter <- forcats::fct_recode(data$parameter, ws = ws)
  }

  if (!is.null(wd) && wd %in% levels(data$parameter)) {
    data$parameter <- forcats::fct_recode(data$parameter, wd = wd)
  }


  if (isFALSE(keep_interval)) {
    data <- dplyr::select(data, -"interval")
  }

  data <- dplyr::group_by(data, .data$parameter, .data$unit)
  keys <- dplyr::group_keys(data)
  if (isFALSE(as_list) && anyDuplicated(keys$parameter) > 0) {
    stop("Found the same parameter with differents units.")
  }

  data <- dplyr::rename(data, date = "starttime")


  if (isTRUE(as_list)) {
    data <- dplyr::group_split(data)
    data <- purrr::map(data, tidyr::spread, "parameter", "value")
  } else {
    data <- dplyr::ungroup(data)
    data <- dplyr::select(data, -"unit")
    data <- tidyr::spread(data, "parameter", "value")
    attr(data, "units") <- rlang::set_names(as.character(keys$unit), as.character(keys$parameter))
  }

  data
}


#' Converts one parameter in a rolf data frame to an openair compatible format
#'
#' This function plucks the data for one parameter. If multiple combinations of parameter, unit and interval exists it
#' is necessary to specify which unit and/or interval to pluck.
#'
#' @param data containing the parameter in rolf format
#' @param parameter to pluck
#' @param unit Optional: unit of the parameter to pluck. Default NULL
#' @param interval Optional: interval of the parameter to pluck. Default NULL
#' @param keep_interval Optional: keep interval column. Default FALSE
#'
#' @return tibble with one parameter in openair compatible format including unit, site and optional interval column
#'
#' @export
#'
#' @examples
#' h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv",
#'                    package = "rOstluft.data", mustWork = TRUE)
#'
#' airmo_h1 <- read_airmo_csv(h1)
#' rolf_to_openair_single(airmo_h1, "NO2", unit = "µg/m3", keep_interval = TRUE)
#'
rolf_to_openair_single <- function(data, parameter = NULL, unit = NULL, interval = NULL, keep_interval = FALSE) {
  filter_list <- purrr::compact(list(parameter = parameter, unit = unit, interval = interval))

  if (length(filter_list) > 0) {
    data <- filter_keep_list(data, filter_list)
  }

  data <- dplyr::group_by(data, .data$parameter, .data$unit, .data$interval)

  if (dplyr::n_groups(data) > 1) {
    stop("Multiple parameter/unit/interval combinations. Use argument parameter, unit and interval to select one")
  }

  data <- dplyr::ungroup(data)
  data <- dplyr::rename(data, date = "starttime")

  if (isFALSE(keep_interval)) {
    data <- dplyr::select(data, -"interval")
  }

  tidyr::spread(data, "parameter", "value")
}


#' Converts data from openair format to rolf
#'
#' @param data in openair format
#' @param site specify site of the data. Necessary if data contains no column "site"
#' @param interval specify interval of the data. Necessary if data contains no column "interval"
#' @param units specify parameter unit mapping as named vector. Name = parameter, value = unit. Necessary if data
#'   contains no column "unit"
#' @param ws renames parameter "ws" to this Value. Default "WVv". Set ws = NULL to disable renaming
#' @param wd renames parameter "wd" to this Value. Default "WD". Set wd = NULL to disable renaming
#'
#' @return Converted data in rolf format
#' @export
#'
#' @examples
#' h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv",
#'                    package = "rOstluft.data", mustWork = TRUE)
#'
#' airmo_h1 <- read_airmo_csv(h1)
#' airmo_h1
#'
#' oa_h1 <- rolf_to_openair(airmo_h1)
#' units_h1 <- attr(oa_h1, "units")
#'
#' # we lost the parameter with ppb/ppm units trough the convertions
#' openair_to_rolf(oa_h1, interval = "h1", units = units_h1)
#'
openair_to_rolf <- function(data, site = NULL, interval = NULL, units = NULL, ws = "WVv", wd = "WD") {
  if (isFALSE(tibble::is_tibble(data))) {
    data <- tibble::as_tibble(data)
  } else {
    data <- dplyr::ungroup(data) # just make sure the data isn't grouped
  }

  if (is.null(interval) && isFALSE(tibble::has_name(data, "interval"))) {
    stop("interval is missing. Please use argument interval")
  }

  if (is.null(site) && isFALSE(tibble::has_name(data, "site"))) {
    stop("site is missing. Please use argument site")
  }

  if (is.null(units) && isFALSE(tibble::has_name(data, "unit"))) {
    stop("unit is missing. Please use argument units")
  }

  if (isFALSE(tibble::has_name(data, "date")) &&
      isFALSE(lubridate::is.POSIXct(data$date)) &&
      lubridate::tz(data$date) == ""
  ) {
    stop("data must contain a POSIXct data column with a tz")
  }

  data <- dplyr::rename(data, starttime = "date")
  parameters <- rlang::names2(data)
  parameters <- purrr::discard(parameters, ~ . %in% c("starttime", "site", "unit", "interval"))


  data <- tidyr::gather(data, "parameter", "value", !!!parameters, factor_key = TRUE)

  # if the argument is null, the row is there, just check it is a factor
  if (isFALSE(is.null(site))) {
    data$site <- forcats::as_factor(site)
  } else if (isFALSE(is.factor(data$site))) {
    data$site <- forcats::as_factor(data$site)
  }

  if (isFALSE(is.null(interval))) {
    data$interval <- forcats::as_factor(interval)
  } else if (isFALSE(is.factor(data$interval))) {
    data$interval <- forcats::as_factor(data$interval)
  }

  if (isFALSE(tibble::has_name(data, "unit"))) {
    par_is_in_unit <- parameters %in% rlang::names2(units)
    if (isFALSE(all(par_is_in_unit))) {
      stop("Unit for following parameters are missing: %s",
           stringr::str_c(parameters[par_is_in_unit], collapse = ", ")
      )
    }
    data$unit <- forcats::as_factor(units[data$parameter])
  } else if (isFALSE(is.factor(data$unit))) {
    data$unit <- forcats::as_factor(data$unit)
  }

  # and the value should be a double
  if (isFALSE(is.double(data$value))) {
    data$value <- as.double(data$value)
  }


  if (!is.null(ws) && "ws" %in% levels(data$parameter)) {
    data$parameter <- forcats::fct_recode(data$parameter, !!ws := "ws")
  }

  if (!is.null(wd) && "wd" %in% levels(data$parameter)) {
    data$parameter <- forcats::fct_recode(data$parameter, !!wd := "wd")
  }

  # at this point we should have a tibble with starttime, site, interval, unit, value
  # and the correct classes, the only thing left ist to reorder the columns
  dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
}
