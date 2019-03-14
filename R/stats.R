#' Calculates most common stats
#'
#' @description
#' Calculates the most common used stats from min10 or min30 input values:
#' - Averages (wind vector and scalar, RainDur Sum): input > h1, d1, m1, y1
#' - min max:  input > d1, m1, y1  &  d1 > m1, y1
#' - n: input > y1  &  d1 > y1
#' - perc95 (perc02 & perc98 for O3): input > y1
#'
#' @param input data in rolf format
#'
#' @return list with stats for h1, d1, m1, y1
#' @export
calculate_stats <- function(input) {
  averages <- list(
    "default_statistic" = "mean",
    "WD" = "wind.direction",
    "WVs" = "wind.speed_scalar",
    "WVv" = "wind.speed_vector",
    "RainDur" = "sum"
  )

  stats_y1_from_inp <- list(
    "default_statistic" =  list("min", "max", "n", "perc95"),
    "RainDur" = "n",
    "O3" = list("min", "max", "n", "perc98", "perc02")
  )

  stats_y1_from_d1 <- list(
    "default_statistic" =  list("min", "max", "n"),
    "RainDur" = "drop"
  )

  # rest: input > d1, m1  | d1 > m1
  stats_min_max <- list(
    "default_statistic" = list("min", "max"),
    "RainDur" = "drop"
  )

  ################# CODE START ##################################
  interval <- dplyr::distinct(input, .data$interval)

  if (nrow(interval) != 1) {
    stop("multiple intervals in input")
  }

  interval <- as.character(interval[[1]])
  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))

  # pad the data one time manual instead of every time
  start_date <- lubridate::floor_date(min(input$starttime), "year")
  end_max <- max(input$starttime)
  end_date <- lubridate::ceiling_date(end_max, "year")
  drop_last <- (end_max != end_date)
  input <- pad(input, start_date, end_date, drop_last)

  # average all the data for different intervals as basis
  h1 <- resample(input, averages, "h1", data_thresh = 0.8, skip_padding = TRUE)
  d1 <- resample(input, averages, "d1", data_thresh = 0.8, skip_padding = TRUE)
  m1 <- resample(input, averages, "m1", data_thresh = 0.8, skip_padding = TRUE)
  y1 <- resample(input, averages, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)

  # calculate the different stats
  y1_from_inp <- resample(input, stats_y1_from_inp, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)
  y1_from_d1 <- resample(d1, stats_y1_from_d1, "y1", data_thresh = 0.8, skip_padding = TRUE)

  # collect some min max
  d1_from_inp <- resample(input, stats_min_max, "d1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_inp <- resample(input, stats_min_max, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_d1 <- resample(d1, stats_min_max, "m1", data_thresh = 0.8, skip_padding = TRUE)

  res <- list(
    h1 = h1,
    d1 = bind_rows_with_factor_columns(d1, d1_from_inp),
    m1 = bind_rows_with_factor_columns(m1, m1_from_inp, m1_from_d1),
    y1 = bind_rows_with_factor_columns(y1, y1_from_inp, y1_from_d1)
  )

  res
}


#' Calculates exceedance of LRV limits
#'
#' @param input data in rolf format
#'
#' @return list with m1 and y1 stats
#' @export
calculate_LRV <- function(input) {
  averages_h1 <- list(
    "default_statistic" = "drop",
    "O3" = "mean"
  )

  averages_d1 <- list(
    "default_statistic" = "drop",
    "CO" = "mean",
    "PM10" = "mean",
    "PM2.5" = "mean",
    "NO2" = "mean",
    "SO2" = "mean"
  )

  O3_d1_on_h1 <- list(
    "default_statistic" = "drop",
    "O3" = "max"
  )

  limits_h1 <- list(
    "default_statistic" = "drop",
    "O3" = "n>120"
  )

  limits_d1 <- list(
    "CO" = "n>8",
    "PM10" = "n>50",
    "PM2.5" = "n>25",
    "NO2" = "n>80",
    "SO2" = "n>100",
    "O3_max_h1" = "n>120"
  )

  # keep only mass concentrations
  input <- dplyr::filter(input, .data$unit == "\u00b5g/m3" | .data$unit == "mg/m3")

  interval <- dplyr::distinct(input, .data$interval)

  if (nrow(interval) != 1) {
    stop("multiple intervals in input")
  }

  interval <- as.character(interval[[1]])
  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))

  # pad the data one time manual instead of every time
  start_date <- lubridate::floor_date(min(input$starttime), "year")
  end_max <- max(input$starttime)
  end_date <- lubridate::ceiling_date(end_max, "year")
  drop_last <- (end_max != end_date)
  input <- pad(input, start_date, end_date, drop_last)

  h1 <- resample(input, averages_h1, "h1", data_thresh = 0.8, skip_padding = TRUE)
  d1 <- resample(input, averages_d1, "d1", data_thresh = 0.8, skip_padding = TRUE)
  d1_O3 <- resample(h1, O3_d1_on_h1, "d1", data_thresh = 0.8, skip_padding = TRUE)
  d1 <- bind_rows_with_factor_columns(d1, d1_O3)

  m1_from_h1 <- resample(h1, limits_h1, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_d1 <- resample(d1, limits_d1, "m1", data_thresh = 0.8, skip_padding = TRUE)
  m1_from_d1 <- dplyr::mutate(m1_from_d1,
    parameter = dplyr::recode(.data$parameter, "O3_max_h1_nb_d1>120" = "O3_nb_d1_h1>120")
  )

  y1_from_h1 <- resample(h1, limits_h1, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)
  y1_from_d1 <- resample(d1, limits_d1, "y1", data_thresh = 0.8, max_gap = max_gap, skip_padding = TRUE)
  y1_from_d1 <- dplyr::mutate(y1_from_d1,
    parameter = dplyr::recode(.data$parameter, "O3_max_h1_nb_d1>120" = "O3_nb_d1_h1>120")
  )

  list(
    m1 = bind_rows_with_factor_columns(m1_from_h1, m1_from_d1),
    y1 = bind_rows_with_factor_columns(y1_from_h1, y1_from_d1)
  )
}


calculate_CO_h8gl <- function(input) {
  if (isFALSE(requireNamespace("openair"))) {
    stop("Package openair is needed")
  }

  # keep only mass concentrations
  input <- dplyr::filter(input, .data$parameter == "CO", .data$unit == "mg/m3")

  interval <- dplyr::distinct(input, .data$interval)

  if (nrow(interval) != 1) {
    stop("multiple intervals in input")
  }

  interval <- as.character(interval[[1]])
  max_gap <- switch(interval, "min10" = 6*24*10, "min30" = 48*10,
                    stop("only supports following intervals: min10, min30"))


  # calculate the rolling mean in openair
  input <- resample(input, "mean", "h1", data_thresh = 0.8)
  input <- rolf_to_openair_single(input)
  input <- openair::rollingMean(input, pollutant = "CO", width = 8, new.name = "CO", data.thresh = 80, align = "left")
  input <- openair_to_rolf(input, interval = "h8gl")

  y1 <- resample(input, "max", "y1", data_thresh = 0.8, max_gap = max_gap)

  list(
    h1 = input,
    y1 = y1
  )
}
