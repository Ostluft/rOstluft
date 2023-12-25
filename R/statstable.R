#' Expands compact statstable to a table with one stats per row
#'
#' For each comma seperated value in a cell a new row is added.
#'
#' @param statstable compact statstable
#' @param sep seperator of combined values
#'
#' @seealso
#' * `tidyr::seperate_rows()` is used to expand the table. One call per column.
#'
#' @examples
#' statstable <- tibble::tribble(
#'   ~parameter, ~statistic, ~from, ~to,
#'   "O3, NO2", "min, max", "h1, d1", "m1, y1"
#' )
#' statstable_expand(statstable)
#'
#' @return expanded statstable
#' @export
statstable_expand <- function(statstable, sep = "\\s*,\\s*") {
  statstable <- tidyr::separate_rows(statstable, "parameter", sep = sep)
  statstable <- tidyr::separate_rows(statstable, "statistic", sep = sep)
  statstable <- tidyr::separate_rows(statstable, "from", sep = sep)
  statstable <- tidyr::separate_rows(statstable, "to", sep  = sep)
  statstable
}


#' Create resample compatible statistic lists
#'
#' Could be exported for user to debug his table
#'
#' @param table statstable
#' @param from interval to generate statistic list
#' @param inputs parameters in input
#'
#' @return list with statistic list for every to
#'
#' @keywords internal
get_statistics_from_table <- function(table, from, inputs = NULL) {
  res <- dplyr::filter(table, .data$from == !!from)
  res <- dplyr::group_by(res, .data$to)
  keys <- dplyr::group_keys(res)
  res <- dplyr::group_split(res)
  res <- purrr::map(res, get_list)
  res <- rlang::set_names(res, keys$to)

  # set default statistic to drop
  res <- purrr::map(res, append_default)

  # expand statistics with input parameters
  if (!is.null(inputs)) {
    res <- purrr::map(res, expand_inputs, inputs = inputs)
  }
}


#' convert table to resample compatible list
#'
#' @param table statstable
#'
#' @return list
#'
#' @keywords internal
get_list <- function(table) {
  table <- dplyr::group_by(table, .data$parameter)
  table <- dplyr::summarise(table, statistic = list(.data$statistic))
  res <- rlang::set_names(table$statistic, table$parameter)
  purrr::map(res, ~ if (length(.x) > 1) { as.list(.x) } else { .x } ) # nolint
}

#' Appends default_statistic drop if necessary
#'
#' @param x statistic list
#'
#' @return statistic list containing a default_statistic
#'
#' @keywords internal
append_default <- function(x) {
  if (is.null(x[["default_statistic"]])) {
    x["default_statistic"] <- "drop"
  }
  x
}

#' Adds statistics for parameters in inputs
#'
#' @param x statistic list
#' @param inputs parameters in input
#'
#' @return expanded statistic list
#'
#' @keywords internal
expand_inputs <- function(x, inputs) {
  if (!is.null(x[["_inputs_"]])) {
    for (parameter in inputs) {
      if (is.null(x[[parameter]])) {
        x[[parameter]] <- x[["_inputs_"]]
      }
    }
    x[["_inputs_"]] <- NULL
  }
  x
}

#' Calculates stats described in a table
#'
#' @description
#' Calculate a lot of statistics defined in a table with various caveats:
#'
#' * series always padded to complete years
#' * the same data threshold for all calculations
#' * to h8gl: only mean from h1
#' * max_gap only by to y1, and definition in **days**
#' * usage of `default_statistic` and `_inputs_` can contain suprises in the result
#'
#' All statistics are defined in a table with the columns "parameter", "statistic", "from" and "to". Each row contains
#' one statistic for one parameter with a basis interval ("from") and the target interval ("to"). The rows are then
#' grouped with "from", then with "to" and then with "parameter". This results in a list of statistic for each
#' parameter. This list is compatible with `resample()`. If no `default_statistic` is defined,
#' `default_statistic = "drop"` is added. Multi-step statistics are possible. `_inputs_` can be used as substitute for
#' `default_statistic` in multi-step calculation if the input in "from" already contains calculated statistics.
#' The statstable can be written in a compact form with comma seperated values in each cells. For each value the table
#' will be expanded and a row added. See `statstable_expand()`
#'
#' @param data input data in rolf format
#' @param statstable description of statistics to calculate in table form
#' @param sep seperator for combined values in statstable
#' @param keep_input should the input data be kept in return list as item input. Default FALSE
#' @param data_thresh minimum data capture threshold 0 - 1.0 to use. Default 0.8
#' @param max_gap in days. Only used in calculation to y1. Set to NULL to disable usage. Default 10 days
#' @param order defines the order of calculation in the from column
#'
#' @return list with one item for every to interval
#' @export
#'
#' @examples
#' # calculate LRV statisitcs
#' lrv_table <- tibble::tribble(
#'   ~parameter, ~statistic, ~from, ~to,
#'   "SO2, NO2, PM10", "mean", "input", "y1",
#'   "SO2, NO2", "perc95", "input", "y1",
#'   "O3", "perc98", "input", "m1",
#'   "O3", "mean", "input", "h1",
#'   "O3", "n>120", "h1", "y1",
#'   "SO2, NO2, CO, PM10", "mean", "input", "d1",
#'   "SO2", "n>100", "d1", "y1",
#'   "NO2", "n>80", "d1", "y1",
#'   "CO", "n>8", "d1", "y1",
#'   "PM10", "n>50", "d1", "y1"
#' )
#'
#' fn <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2017.csv",
#'                    package = "rOstluft.data", mustWork = TRUE)
#'
#' data <- read_airmo_csv(fn)
#'
#' # convert volume concentrations to mass concentrations
#' data <- calculate_mass_concentrations(data)
#'
#' stats <- calculate_statstable(data, lrv_table)
#'
#' # we are only interested in the m1 and y1 results
#' stats <- dplyr::bind_rows(stats$y1, stats$m1)
#' stats
#'
#' # calculate clima indicators
#' clima_table <- tibble::tribble(
#'    ~parameter, ~statistic, ~from, ~to,
#'    "T", "mean", "input", "h1",
#'    "T", "max, min", "h1", "d1",
#'    "T_max_h1", "Sommertage, Hitzetage, Eistage", "d1", "y1",
#'    "T_min_h1", "Tropennächte, Frosttage", "d1", "y1",
#' )
#' clima_stats <- calculate_statstable(data, clima_table)
#' clima_stats$y1
calculate_statstable <- function(data, statstable, sep = "\\s*,\\s*", keep_input = FALSE, data_thresh = 0.8,
                                 max_gap = 10, order = c("input", "h1", "h8gl", "d1", "m1", "y1")) {

  # hand over calculation of h8gl to open air.
  calc_h8gl <- function(parameter, data) {
    data <- dplyr::filter(data, .data$parameter == !!parameter)

    if (nrow(data) < 8) {
      data[0, ]
    } else {
      data <- dplyr::arrange(data, .data$starttime)
      data$value <- .Call("rollMean", data$value, 8, data_thresh * 100, "left", PACKAGE = "openair")
      data <- dplyr::mutate(data, interval = as.factor("h8gl"))
      data
    }
  }


  # wrapper around resample  to handle exceptions for max_gap and h8gl
  calc_stats <- function(data, stats, to, from) {
    if (to == "y1" && !is.null(max_gap) && from %in% c("input", "min10", "min30", "h1", "d1")) {
      interval <- as.character(dplyr::first(data[[from]]$interval)) # take the interval from the data for case input
      max_gap <- switch(interval, "min10" = 144, "min30" = 48, "h1" = 24, "d1" = 1) * max_gap
      result <- resample(data[[from]], stats, to,  skip_padding = TRUE, data_thresh = data_thresh, max_gap = max_gap)
    } else if (to == "h8gl") {
      if (from != "h1") {
        stop("h8gl can only calculated from h1")
      }
      if (isFALSE(suppressMessages(requireNamespace("openair")))) {
        stop("Package openair is needed to calculate rollings means")
      }
      # need to remove the automatic added default_statistics
      # this implementation should be refactored too many iterations over the complete data
      # it would be better to filter first, then loop over roolingMean only
      parameters <- purrr::keep(rlang::names2(stats), ~ .x != "default_statistic")
      result <- purrr::map(parameters, calc_h8gl, data[[from]])
      result <- dplyr::bind_rows(!!!result)
    } else {
      result <- resample(data[[from]], stats, to,  skip_padding = TRUE, data_thresh = data_thresh)
    }

    data[[to]] <- dplyr::bind_rows(data[[to]], result)
    data
  }

  ####### FUNCTION START #####

  # expand statstable, create for a row for every combined cell
  # probably not reasonable for all columns
  statstable <- statstable_expand(statstable, sep)

  # prepare res for reduce/loop
  intervals <- dplyr::distinct(statstable, .data$to)[["to"]]
  res <- purrr::map(intervals, ~ data[0, ])
  res <- rlang::set_names(res, intervals)
  detect_interval(data)  # use detect interval to be sure only one interval is in data
  input_params <- as.character(dplyr::distinct(data, .data$parameter)[["parameter"]])
  res$input <- pad_year(data)

  # we need to seperate wind stats and all other, because we cannot combine them
  # for restrictions in resample
  is_wind_stat <- stringr::str_starts(statstable$statistic, "wind.")
  wind_stats <- statstable[is_wind_stat, ]
  statstable <- statstable[!is_wind_stat, ]

  # get needed steps to calculate all stats
  loop_order <- dplyr::intersect(order, wind_stats$from)
  for (from in loop_order) {
    stats <- get_statistics_from_table(wind_stats, from, input_params)
    res <- purrr::reduce2(stats, names(stats), calc_stats, .init = res, from = from)
  }

  # get needed steps to calculate all stats
  loop_order <- dplyr::intersect(order, statstable$from)
  for (from in loop_order) {
    stats <- get_statistics_from_table(statstable, from, input_params)
    res <- purrr::reduce2(stats, names(stats), calc_stats, .init = res, from = from)
  }

  if (!keep_input) {
    res[["input"]] <- NULL
  }
  res
}
