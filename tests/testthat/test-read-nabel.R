context("read nabel txt file")

# minimalistic testing
#

test_that("correct content", {
  due <- system.file("extdata", "nabel_due.txt", package = "rOstluft.data", mustWork = TRUE)
  res <- read_nabel_txt(due, na.rm = FALSE)

  testthat::expect_equal(dim(res), c( 13 * 7, 6))
  testthat::expect_equal(sum(is.na(res$value)), 8)

  testthat::expect_equal(min(res$starttime), lubridate::ymd_hms("2017-01-01T00:00:00", tz = "Etc/GMT-1"))
  testthat::expect_equal(max(res$starttime), lubridate::ymd_hms("2017-12-31T23:00:00", tz = "Etc/GMT-1"))
  testthat::expect_equal(as.character(dplyr::first(res$site)), "Dübendorf-Empa")

  testthat::expect_true(
    dplyr::setequal(levels(res$parameter), c("NOx", "NO2", "PM10_ber", "TEMP", "WIRI", "WIGE", "STRGLO"))
  )

  testthat::expect_true(
    dplyr::setequal(levels(res$unit), c("ppb", "ug/m3", "°C", "°", "m/s", "W/m2"))
  )

  testthat::expect_true(
    dplyr::setequal(levels(res$interval), c("h1"))
  )


  # manual interval, makes manual shifting necessary
  testthat::expect_error(
    res <- read_nabel_txt(due, interval = "10 minutes", na.rm = FALSE)
  )


  res <- read_nabel_txt(due, interval = "10 minutes", time_shift = lubridate::period(+10, units = "minutes"))
  testthat::expect_equal(res$starttime[1], lubridate::ymd_hms("2017-01-01T01:10:00", tz = "Etc/GMT-1"))
  testthat::expect_equal(as.character(res$interval[1]), "10 minutes" )

})
