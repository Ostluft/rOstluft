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
                                  parameter %in% c("WD", "WVv"))

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

  # just be sure it runs with alot of data
  #resample(input, statistic = list("WD" = "vector.avg.wd", "WVv" = "vector.avg.ws", "RainDur" = "sum"), "d1")


  # problem we have no comparision data for 1d wind
  # TODO wind test cases
  #wind_d1 <- resample(wind_inp_min30, list("WD" = "vector.avg.wd", "WVv" = "vector.avg.ws"), "d1")

})
