#' Factory to generate statistical functions
#'
#' This factory adds wrapper around statistical methods to handle na values. Additional it provides a simple
#' way to apply a minimum required data capture rate to any function.
#'
#' @param statistic Statistical method to generate function. Can be one of “mean”, “max”, “min”, “median”, "sum",
#'   “n”, “sd”, “percentile”. Note that “sd” is the standard deviation, “n” is the number
#'   (frequency) of valid records in the period and “data.cap” is the percentage data capture. “percentile”
#'   is the percentile level (\%) between 0-100, which can be set using the “percentile” argument.
#'   Or a function with one argument expecting a vector.
#' @param threshold optional minimum data capture threshold in \% to use
#' @param percentile The percentile level in \% used when statistic = "percentile". The default is 95.
#'
#' @return statistic function with one argument
#' @export
statistic_fun_factory <- function(statistic, threshold = NULL, percentile = 95) {
  statistic_fun <- NULL

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
                            "vector.avg.ws" = statistic_vector_average_ws,
                            "vector.avg.wd" = statistic_vector_average_wd,
                            NULL
    )
  }

  if (is.null(statistic_fun)) {
    stop("Statistic not recognised")
  }

  if (!is.null(threshold)) {
    if (is.numeric(threshold) && threshold >= 0 && threshold <= 1.0) {
      return(treshold_wrapper_function(statistic_fun, threshold))
    } else {
      stop("Data threshold range outside 0 - 1.0")
    }
  }

  statistic_fun
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

#' @rdname statistic_fun
#' @keywords internal
statistic_vector_average_ws <- function(ws, wd) {
  Uu <- mean(ws * sin(2 * pi * wd / 360), na.rm = TRUE) # borrowed from openair::timeAverage, modified
  Vv <- mean(ws * cos(2 * pi * wd / 360), na.rm = TRUE)
  sqrt((Uu^2 + Vv^2))
}

#' @rdname statistic_fun
#' @keywords internal
statistic_vector_average_wd <- function(ws, wd) {
  Uu <- mean(ws * sin(2 * pi * wd / 360), na.rm = TRUE)   # borrowed from openair::timeAverage, modified
  Vv <- mean(ws * cos(2 * pi * wd / 360), na.rm = TRUE)
  (atan2(Uu, Vv) * 360 / 2 / pi) %% 360
}


