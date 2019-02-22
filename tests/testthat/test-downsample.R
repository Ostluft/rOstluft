context("downsample")

test_that("basis statistic", {
  staba_min30 <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",
                             package = "rOstluft.data", mustWork = TRUE)

  staba_1d <- system.file("extdata", "Zch_Stampfenbachstrasse_d1_2013_Jan.csv",
                          package = "rOstluft.data", mustWork = TRUE)

  staba_1d_nox <-  system.file("extdata", "Zch_Stampfenbachstrasse_d1_NOx_2013_Jan.csv",
                               package = "rOstluft.data", mustWork = TRUE)


  start <- lubridate::as_datetime("2013-01-01", tz = "Etc/GMT-1")
  end <-  lubridate::as_datetime("2013-01-31T23:59:00", tz = "Etc/GMT-1")


  input <- read_airmo_csv(staba_min30)
  rd_inp_min30 <- dplyr::filter(input, dplyr::between(.data$starttime, start, end), .data$parameter == "RainDur")
  nox_inp_min30 <- dplyr::filter(input, dplyr::between(.data$starttime, start, end), .data$parameter == "NOx")
  inp_min30 <- bind_rows_with_factor_columns(nox_inp_min30, rd_inp_min30)

  wind_inp_min30 <- dplyr::filter(input, dplyr::between(.data$starttime, start, end),
                                  parameter %in% c("WD", "WVv", "WVs"))

  output <- read_airmo_csv(staba_1d_nox)

  # single series
  testthat::expect_equivalent(
    resample(nox_inp_min30, "mean", "d1")$value,
    dplyr::filter(output, .data$parameter == "NOx")$value
  )

  testthat::expect_equal(
    resample(nox_inp_min30, "min", "d1", rename.parameter = TRUE)$value,
    dplyr::filter(output, .data$parameter == "NOx_min_min30")$value
  )

  testthat::expect_equal(
    resample(nox_inp_min30, "max", "d1")$value,
    dplyr::filter(output, .data$parameter == "NOx_max_min30")$value
  )

  testthat::expect_equal(
    resample(nox_inp_min30, "n", "d1")$value,
    dplyr::filter(output, .data$parameter == "NOx_nb_min30")$value
  )

  testthat::expect_equal(
    resample(rd_inp_min30, "sum", "d1")$value,
    dplyr::filter(output, .data$parameter == "RainDur")$value
  )

  testthat::expect_equal(
    nrow(resample(rd_inp_min30, "drop", "d1")),
    0
  )

  testthat::expect_equal(
    nrow(resample(rd_inp_min30, dplyr::first, "d1")),
    31
  )

  # mulitple series
  inp_d1 <- resample(inp_min30, list("RainDur" = "sum"), "d1")

  testthat::expect_equal(
    dplyr::filter(inp_d1, .data$parameter == "RainDur")$value,
    dplyr::filter(output, .data$parameter == "RainDur")$value
  )

  testthat::expect_equal(
    dplyr::filter(inp_d1, .data$parameter == "NOx")$value,
    dplyr::filter(output, .data$parameter == "NOx")$value
  )

  inp_d1 <- resample(inp_min30, list("RainDur" = "sum", default_statistic = "max"), "d1", rename.parameter = TRUE)
  testthat::expect_equal(
    dplyr::filter(inp_d1, .data$parameter == "NOx_max_min30")$value,
    dplyr::filter(output, .data$parameter == "NOx_max_min30")$value
  )

  testthat::expect_equal(
    nrow(resample(inp_min30, "drop", "d1")),
    0
  )

  testthat::expect_equal(
    nrow(resample(inp_min30, dplyr::first, "d1")),
    62
  )

  # wind resampling only functional check not correctness of math, the math is simple anyway
  testthat::expect_error(
    resample(wind_inp_min30, list("WD" = "wind.direction", "WVv" = "wind.speed_vector"), "d1")
  )

  statistics <- list(
    "WD" = "wind.direction",
    "WVv" = "wind.speed_vector",
    "WVs" = "wind.speed_scalar",
    "RainDur" = "sum",
    "CO" = "drop",
    "O3" = "max",
    "T" = "min",
    "PM10" = "median",
    "Hr" = "sd",
    "NO" = "n",
    "NO2" = "coverage",
    "NOX" = "percentile",
    "StrGlo" = "mean",
    "SO2" = list("mean", "min", "max", "n", "coverage"),
    "p" = dplyr::first,
    "default_statistic" = "n"
  )

  testthat::expect_equal(
    nrow(resample(wind_inp_min30, statistics, "d1")),
    93
  )

  # multiple statistics without renaming
  testthat::expect_error(
    resample(inp_min30, list("default_statistic" = list("mean", "median" ,"min", "max", "n", "coverage")), "d1",
             rename.parameter = TRUE)
  )

  # multiple statistics without renaming
  testthat::expect_error(
    resample(inp_min30, list("default_statistic" = list("mean", "median" ,"min", "max", "n", "coverage")), "d1",
             rename.parameter = TRUE)
  )

  # mutliple statistics renaming flag false
  testthat::expect_error(
    resample(inp_min30, list("default_statistic" = list("mean","min", "max", "n", "coverage")), "d1",
             rename.parameter = FALSE)
  )

  # just be sure it runs with alot of data, all statistics and supplied function, renames
  res <- resample(input, statistics, "y1", rename.parameter = TRUE, data.thresh = 0.8, max_gap = 480)

  testthat::expect_is(
    res, "tbl_df"
  )

  testthat::expect_is(
    res$site, "factor"
  )

  testthat::expect_is(
    res$parameter, "factor"
  )

  testthat::expect_is(
    res$interval, "factor"
  )

  testthat::expect_is(
    res$unit, "factor"
  )

})
