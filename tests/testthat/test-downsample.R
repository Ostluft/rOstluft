context("downsample")

min30 <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2013_Jan.csv",
                     package = "rOstluft.data", mustWork = TRUE)

d1 <- system.file("extdata", "Zch_Stampfenbachstrasse_d1_2013_Jan.csv",
                  package = "rOstluft.data", mustWork = TRUE)

h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv",
                  package = "rOstluft.data", mustWork = TRUE)

y1 <- system.file("extdata", "Zch_Stampfenbachstrasse_y1_2010-2014.csv",
                  package = "rOstluft.data", mustWork = TRUE)

min30_y1 <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",
                        package = "rOstluft.data", mustWork = TRUE)


# drop ppb units for simpler filtering in expect_equal_values

airmo_min30 <- read_airmo_csv(min30)
airmo_min30 <- dplyr::filter(airmo_min30, unit != "ppb")

airmo_h1 <- read_airmo_csv(h1, na.rm = FALSE)
airmo_h1 <- dplyr::filter(airmo_h1, unit != "ppb")

airmo_d1 <- read_airmo_csv(d1, na.rm = FALSE)
airmo_d1 <- dplyr::filter(airmo_d1, unit != "ppb")

airmo_y1 <-  read_airmo_csv(y1, na.rm = FALSE)
airmo_min30_y1 <- read_airmo_csv(min30_y1)


print_fun <- function(x, ...) {
  print("\n")
  print(x, ...)

}


expect_equal_values <- function(input, output, parameter) {
  testthat::expect_equivalent(
    dplyr::filter(input, .data$parameter == !!parameter)$value,
    dplyr::filter(output, .data$parameter == !!parameter)$value,
    label = parameter
  )
}


test_that("min30 to d1", {
  # the file contains mass and parts concentration, to simplify filtering for test only use parameters with one unit
  # but the min calculation in airmo is wrong, and we got false values for T, Hr, p, PM10 => filter ppb units and use
  # O3 [µg/m3] ...
  statistics <- list(
    "default_statistic" = "drop",
    "Hr" = "mean",
    "RainDur" = "sum",
    "O3" = list("mean", "max", "min", "n")
  )

  res_min30_to_d1 <- resample(airmo_min30, statistics, "d1", data_thresh = 0.8)

  testthat::expect_equal(
    nrow(res_min30_to_d1),
    (length(purrr::flatten(statistics)) - 1) * 31  # don't count drop
  )

  for (par in levels(droplevels(res_min30_to_d1$parameter))) {
    expect_equal_values(res_min30_to_d1, airmo_d1, par)
  }
})

test_that("min30 to h1", {
  statistics <- list(
    "default_statistic" = "drop",
    "WD" = "wind.direction",
    "WVs" = "wind.speed_scalar",
    "WVv" = "wind.speed_vector",
    "RainDur" = "sum",
    "NO" = "mean",
    "SO2" = "mean",
    "NO2" = "mean"
  )

  res_min30_to_h1 <-  resample(airmo_min30, statistics, "h1", data_thresh = 0.8)

  testthat::expect_equal(
    nrow(res_min30_to_h1),
    (length(purrr::flatten(statistics)) - 1) * 31 * 24  # don't count drop
  )

  pars <- levels(droplevels(res_min30_to_h1$parameter))
  pars <- purrr::discard(pars, ~.x == "WVs")  # no data for WVs in Airmo data

  for (par in pars) {
    expect_equal_values(res_min30_to_h1, airmo_h1, par)
  }
})

test_that("to y1", {
  min30_statistics <- list(
    "O3" = list("mean", "perc98", "n", "max", "min")
  )

  h1_statistics <- list(
    "O3" = list("max", "min", "n", "n>120", "n>160", "n>180", "n>200", "n>240")
  )

  d1_statistics <- list(
    "O3" = list("max", "min", "n", "n>65")
  )


  O3_min30 <- dplyr::filter(airmo_min30_y1, parameter == "O3")

  O3_h1 <- resample(O3_min30, "mean", "h1", data_thresh = 0.8)
  O3_d1 <- resample(O3_min30, "mean", "d1", data_thresh = 0.8)


  res_O3_min30 <- resample(O3_min30, min30_statistics, "y1", data_thresh = 0.8, max_gap = 480)
  res_O3_h1 <- resample(O3_h1, h1_statistics, "y1")
  res_O3_d1 <- resample(O3_d1, d1_statistics, "y1")

  res <- bind_rows_with_factor_columns(res_O3_min30, res_O3_h1, res_O3_d1)

  pars <- levels(droplevels(res$parameter))
  pars <- purrr::discard(pars, ~.x == "O3_98%_min30")  # airmo omits basis interval when renaming percentile ...

  for (par in pars) {
    expect_equal_values(res, airmo_y1, par)
  }

  # check percentile manually
  testthat::expect_equivalent(
    dplyr::filter(res, .data$parameter == "O3_98%_min30")$value,
    dplyr::filter(airmo_y1, .data$parameter == "O3_98%")$value
  )
})

test_that("get_gap_in_vector", {
  n <- 17500
  i <- 1:n / 10

  i_start <- i_end <- i_middle <-  i
  i_start[1:50] <- NA
  i_middle[5000:6000] <- NA
  i_end[n - 0:200] <- NA
  i_comb <- i_start + i_middle + i_start

  testthat::expect_equal(
    get_gap_in_vector(i),
    0
  )

  testthat::expect_equal(
    get_gap_in_vector(i_start),
    sum(is.na(i_start))
  )

  testthat::expect_equal(
    get_gap_in_vector(i_middle),
    sum(is.na(i_middle))
  )

  testthat::expect_equal(
    get_gap_in_vector(i_end),
    sum(is.na(i_end))
  )

  testthat::expect_equal(
    get_gap_in_vector(i_comb),
    sum(is.na(i_middle))
  )
})

test_that("max_gap", {
  FUN <- statistic_fun_factory("min", max_gap = 480)

  n <- 17500
  i <- 1:n / 10

  i_start <- i_end <- i_middle <-  i
  i_start[1:480] <- NA
  i_middle[5000:6000] <- NA
  i_end[n - 0:480] <- NA
  i_comb <- i_start + i_middle + i_start

  testthat::expect_equal(
    FUN(i),
    0.1
  )

  testthat::expect_equal(
    FUN(i_start),
    0.1 * 481
  )

  testthat::expect_equal(
    FUN(i_middle),
    NA
  )

  testthat::expect_equal(
    FUN(i_end),
    NA
  )

  testthat::expect_equal(
    FUN(i_comb),
    NA
  )
})

test_that("threshold", {
  FUN <- statistic_fun_factory("min", threshold = 0.8)

  i <- 1:2 / 10
  testthat::expect_equal(
    FUN(i),
    0.1
  )

  i[1] <- NA
  testthat::expect_equal(
    FUN(i),
    NA
  )

  i <- 1:48 / 10
  i_ok <- i_fail <- i
  i_ok[10:18] <- NA
  i_fail[10:19] <- NA

  testthat::expect_equal(
    FUN(i),
    FUN(i_ok)
  )

  testthat::expect_equal(
    FUN(i_fail),
    NA
  )
})

test_that("threshold & gap", {
  FUN <- statistic_fun_factory("min", threshold = 0.8, max_gap = 480)

  n <- 17500
  i <- 1:n / 10

  i_ok <- i_fail_threshold <- i
  i_ok[c(1:50, 500:505, 1111:1222, 4321:4345, 9000:9009, 12121:12222, 13333:13444, 15555:15556)] <- NA
  i_fail_gap <- i_ok
  i_fail_gap[10000:10481] <- NA

  for (j in 1:20) {
    i_fail_threshold[j * 750 + 1:300] <- NA
  }

  testthat::expect_equal(
    FUN(i),
    0.1
  )

  testthat::expect_equal(
    FUN(i_ok),
    0.1 * 51
  )

  testthat::expect_equal(
    FUN(i_fail_gap),
    NA
  )

  testthat::expect_equal(
    FUN(i_fail_threshold),
    NA
  )
})
