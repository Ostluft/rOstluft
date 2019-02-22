#' Factory to generate statistical functions
#'
#' This factory adds wrapper around statistical methods to handle na values. Additional it provides a simple
#' way to apply a minimum required data capture rate to any function.
#'
#' @param statistic Statistical method to generate function. Can be one of “mean”, “max”, “min”, “median”, "sum",
#'   “n”, “sd”, “percentile”. Note that “sd” is the standard deviation, “n” is the number
#'   (frequency) of valid records in the period and “coverage” is the percentage data coverage “percentile”
#'   is the percentile level (\%) between 0-1, which can be set using the “percentile” argument.
#'   Or a function with one argument expecting a vector.
#' @param threshold optional minimum data capture threshold 0 - 1.0 to use
#' @param percentile The percentile level in \% used when statistic = "percentile". The default is 0.95.
#' @param max_gap optional maxium Number of consecutive NA values
#'
#' @return statistic function with one argument
#' @export
statistic_fun_factory <- function(statistic, percentile = 0.95, threshold = NULL, max_gap = NULL) {
  statistic_fun <- statistic_fun_1 <- statistic_fun_2 <- NULL

  if (is.function(statistic)) {
    statistic_fun <- statistic
  } else {
    statistic_fun <- switch(statistic,
                            "mean" = statistic_mean,
                            "median" = statistic_median,
                            "sd" = statistic_sd,
                            "n" = statistic_n,
                            "sum" = statistic_sum,
                            "max" = statistic_max,
                            "min" = statistic_min,
                            "coverage" = statistic_coverage,
                            "percentile" = get_statistic_percentile(percentile),
                            NULL
    )
  }

  if (is.null(statistic_fun)) {
    stop("Statistic not recognised")
  }

  # PROBLEM if we reassing statistic_fun <- treshold_wrapper_function(statistic_fun, threshold) we create a recursive
  # call. Probably there is a simple, elegant solution but i'm too stupid ...

  if (!is.null(threshold)) {
    if (is.numeric(threshold) && threshold >= 0 && threshold <= 1.0) {
      statistic_fun_1 <- treshold_wrapper_function(statistic_fun, threshold)
    } else {
      stop("Data threshold range outside 0 - 1.0")
    }
  }

  if (!is.null(max_gap)) {
    if (is.numeric(max_gap) && max_gap > 0 ) {
      if (is.null(statistic_fun_1)) {
        statistic_fun_1 <- gap_wrapper_function(statistic_fun, max_gap)
      } else {
        statistic_fun_2 <- gap_wrapper_function(statistic_fun_1, max_gap)
      }

    } else {
      stop("max_gap should be greater then 0")
    }
  }

  res <- statistic_fun

  if (!is.null(statistic_fun_1)) {
    res <- statistic_fun_1
  }

  if (!is.null(statistic_fun_2)) {
    res <- statistic_fun_2
  }

  res
}

#' Wrapper function for minimum capture threshold
#'
#' @param statistic_fun statistic function to apply if minimum capture threshold is meet
#'
#' @return wrapped function
#'
#' @rdname statistic_fun_factory
#' @export
treshold_wrapper_function <- function(statistic_fun, threshold) {
  function(x) {
    limit <- 1 - threshold
    if (sum(is.na(x)) / length(x) <= limit) {
      statistic_fun(x)
    } else {
      NA
    }
  }
}


#' wrapper function for maximum number of consecutive NA values
#'
#' @param statistic_fun statistic function to apply if max gap criterium is meet
#'
#' @rdname statistic_fun_factory
#' @export
gap_wrapper_function <- function(statistic_fun, max_gap) {
  function(x) {
    if (get_gap_in_vector(x) <= max_gap) {
      statistic_fun(x)
    } else {
      NA
    }
  }
}



#' Get the biggest number of consecutive NA Values
#'
#' Probably not the fastest way to do it. Compared with a loop approach. The loop is a lot faster for short vectors.
#' But for 17500 elements (1 year min30 data) this is faster. But In the end the perfomance don't really matters.
#'
#' @param x Vector
#'
#' @return number longest consecutive NA values
#'
#' @rdname statistic_fun
#' @keywords internal
get_gap_in_vector <- function(x) {
  n <- length(x)
  x <- tibble::tibble(x = x)
  x <- tibble::rowid_to_column(x)
  x <- c(x$rowid[!is.na(x$x)], n+1)
  max(x-dplyr::lag(x, default = 0)) - 1
}

#' @title statistic functions
#' @rdname statistic_fun
#' @keywords internal
statistic_mean <- function(x) {
  mean(x, na.rm = TRUE)
}

#' @rdname statistic_fun
#' @keywords internal
statistic_median <- function(x) {
  stats::median(x, na.rm = TRUE)
}

#' @rdname statistic_fun
#' @keywords internal
statistic_sd <- function(x) {
  stats::sd(x, na.rm = TRUE)
}

#' @rdname statistic_fun
#' @keywords internal
statistic_n <- function(x) {
  length(stats::na.omit(x))
}

#' @rdname statistic_fun
#' @keywords internal
statistic_sum <- function(x) {
  if (all(is.na(x))) {
    NA
  } else {
    sum(x, na.rm = TRUE)
  }
}

#' @rdname statistic_fun
#' @keywords internal
statistic_max <- function(x) {
  if (all(is.na(x))) {
    NA
  } else {
    max(x, na.rm = TRUE)
  }
}

#' @rdname statistic_fun
#' @keywords internal
statistic_min <- function(x) {
  if (all(is.na(x))) {
    NA
  } else {
    min(x, na.rm = TRUE)
  }
}

#' @rdname statistic_fun
#' @keywords internal
statistic_coverage <- function(x) {
  if (all(is.na(x))) {
    res <- 0
  } else {
    res <- (1 - sum(is.na(x)) / length(x))
  }
  res
}

#' @rdname statistic_fun
#' @keywords internal
get_statistic_percentile <- function(percentile) {
  if (!is.numeric(percentile) || percentile < 0 || percentile > 1.0) {
    stop("percentile outside 0 - 1.0")
  }
  function(x) {
    stats::quantile(x, probs = percentile, na.rm = TRUE)
  }
}


