
test_that("read csv file", {
  d1 <- system.file("extdata", "Zch_Stampfenbachstrasse_d1_2013_Jan.csv", package = "rOstluft.data", mustWork = TRUE)
  res <- read_airmo_csv(d1, na.rm = FALSE)
  testthat::expect_equal(dim(res), c(31 * 70, 6)) # 70 Kanäle, 31 Tage
  testthat::expect_equal(sum(is.na(res$value)), 0)
  testthat::expect_equal(levels(res$site), "Zch_Stampfenbachstrasse")
  testthat::expect_equal(levels(res$interval), "d1")
  testthat::expect_equal(length(levels(res$parameter)), 65)
  testthat::expect_equal(length(levels(res$unit)), 11)

  fn <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2013_Jan.csv", package = "rOstluft.data", mustWork = TRUE)
  res <- read_airmo_csv(fn, na.rm = FALSE)
  testthat::expect_equal(dim(res), c(31 * 48 * 19, 6)) # 19 Kanäle, 31 Tage, 48 Werte pro Tag
  testthat::expect_equal(sum(is.na(res$value)), 38)
  testthat::expect_equal(levels(res$site), "Zch_Stampfenbachstrasse")
  testthat::expect_equal(levels(res$interval), "min30")
  testthat::expect_equal(length(levels(res$parameter)), 14)
  testthat::expect_equal(length(levels(res$unit)), 11)
})

test_that("read dat file", {
  fn <- system.file("extdata", "AIRMO_CO_min30_2018.dat", package = "rOstluft.data", mustWork = TRUE)
  res <- read_airmo_dat(fn, na.rm = FALSE)
  testthat::expect_equal(dim(res), c(365 * 48 * 20, 6))  # 20 Kanäle, 365 Tage, 48 Werte pro Tag
  testthat::expect_equal(sum(is.na(res$value)), 321331)
  testthat::expect_equal(length(levels(res$site)), 20)
  testthat::expect_equal(levels(res$interval), "min30")
  testthat::expect_equal(levels(res$parameter), "CO")
  testthat::expect_equal(levels(res$unit), "ppm")
})

test_that("read web export file", {
  fn <- rOstluft.data::f("ol_nox_covid19_2020.csv")
  res <- read_airmo_webexport(fn, na.rm = FALSE)
  testthat::expect_equal(dim(res), c(366 * 48 * 16, 6))  # 16 Kanäle, 366 Tage, 48 Werte pro Tag
  testthat::expect_equal(sum(is.na(res$value)), 220172)
  testthat::expect_equal(length(levels(res$site)), 8)
  testthat::expect_equal(levels(res$interval), "min30")
  testthat::expect_equal(levels(res$parameter), c("NOx", "NO2"))
  testthat::expect_equal(levels(res$unit), "ppb")
})

