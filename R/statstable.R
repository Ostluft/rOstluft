#' Calculates stats described in a table
#'
#' @description
#' With variues caveats:
#'
#' * data_thresh = 0.8 for all stats
#' * No combining wind averaging with other stats. they must have there own row
#' * to h8gl: only mean from h1
#' * min10, min30, h1, d1 > y1 always use max_gap
#'
#' @param data input data in rolf format
#' @param statstable description of statistics to calculate in table form
#' @param sep seperator for combined values in statstable
#' @param order defines the order of calculation in the from column
#'
#' @return list with one item per to interval
#' @export
calculate_statstable <- function(data, statstable, sep = ",",
                                 order = c("input", "h1", "h8gl", "d1", "m1", "y1")) {
  ####### HELPER FUNCTIONS  #######

  # create statistic list for resample for from interval from table
  get_statistics_from_table <- function(table, from) {
    res <- dplyr::filter(table, .data$from == !!from)
    res <- dplyr::group_by(res, .data$to)
    keys <- dplyr::group_keys(res)
    res <- dplyr::group_split(res)
    res <- purrr::map(res, get_list)
    res <- rlang::set_names(res, keys$to)

    # set default statistic to drop
    purrr::map(res, append_default)
  }

  # convert table to resample compatible list
  get_list <- function(table) {
    table <- dplyr::group_by(table, parameter)
    table <- dplyr::summarise(table, statistic = list(.data$statistic))
    res <- rlang::set_names(table$statistic, table$parameter)
    purrr::map(res, ~ if (length(.x) > 1) { as.list(.x) } else { .x } )
  }

  append_default <- function(x) {
    if (is.null(x$default_statistic)) {
      x["default_statistic"] <- "drop"
    }
    x
  }


  # hand over calculation of h8gl to open air.
  calc_h8gl <- function(parameter, data) {
    result <- rolf_to_openair_single(data, parameter)
    result <- openair::rollingMean(result, pollutant = parameter, new.name = parameter,
                                   width = 8, align = "left", data.thresh = 80)
    openair_to_rolf(result, interval = "h8gl")
  }


  # wrapper around resample  to handle exceptions for max_gap and h8gl
  calc_stats <- function(data, stats, to, from) {
    if (to == "y1" && from %in% c("min10", "min30", "h1", "d1")) {
      max_gap <- switch(from, "min10" = 1440, "min30" = 480, "h1" = 240, "d1" = 10)
      result <- resample(data[[from]], stats, to,  skip_padding = TRUE, data_thresh = 0.8, max_gap = max_gap)
    } else if (to == "h8gl") {
      if (from != "h1") {
        stop("h8gl can only calculated from h1")
      }
      if (isFALSE(requireNamespace("openair"))) {
        stop("Package openair is needed")
      }
      # need to remove the automatic added default_statistics
      # this implementation should be refactored too many iterations over the complete data
      # it would be better to filter first, then loop over roolingMean only
      parameters <- purrr::keep(rlang::names2(stats), ~ .x != "default_statistic")
      result <- purrr::map(parameters, calc_h8gl, data[[from]])
      result <- bind_rows_with_factor_columns(!!!result)
    } else {
      result <- resample(data[[from]], stats, to,  skip_padding = TRUE, data_thresh = 0.8)
    }

    data[[to]] <- bind_rows_with_factor_columns(data[[to]], result)
    data
  }

  ####### CODE START #####

  # expand statstable, create for a row for every combined cell
  # probably not reasonable for all columns
  statstable <- tidyr::separate_rows(statstable, parameter, sep = sep)
  statstable <- tidyr::separate_rows(statstable, statistic, sep = sep)
  statstable <- tidyr::separate_rows(statstable, from, sep = sep)
  statstable <- tidyr::separate_rows(statstable, to, sep  = sep)

  # prepare res for reduce/loop
  intervals <- dplyr::distinct(statstable, to)[["to"]]
  res <- purrr::map(intervals, ~ data[0, ])
  res <- rlang::set_names(res, intervals)
  res$input <- pad_input(input)

  # we need to seperate wind stats and all other, because we cannot combine them
  # for restrictions in resample
  is_wind_stat <- stringr::str_starts(statstable$statistic, "wind.")
  wind_stats <- statstable[is_wind_stat, ]
  statstable <- statstable[!is_wind_stat, ]

  # get needed steps to calculate all stats
  order <- dplyr::intersect(order, wind_stats$from)
  for (from in order) {
    stats <- get_statistics_from_table(wind_stats, from)
    res <- purrr::reduce2(stats, names(stats), calc_stats, .init = res, from = from)
  }

  # get needed steps to calculate all stats
  order <- dplyr::intersect(order, statstable$from)
  for (from in order) {
    stats <- get_statistics_from_table(statstable, from)
    res <- purrr::reduce2(stats, names(stats), calc_stats, .init = res, from = from)
  }
  res
}

