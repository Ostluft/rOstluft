test_that("read files", {
  chn_fn <- system.file("extdata", "IAC-Met_2010-12-03.dat", package = "rOstluft.data", mustWork = TRUE)
  hberg_fn <- system.file("extdata", "IAC-Met-HBerg_2012-06-20.dat", package = "rOstluft.data", mustWork = TRUE)

  chn <- read_ethz_iac(chn_fn)  # wellformed file with a value at every interval for 13 parameters
  hberg <- read_ethz_iac(hberg_fn, tz = "Etc/GMT-1", na.rm = FALSE) # wellformed file with na values for 14 parameters

  testthat::expect_equal(nrow(chn), 6 * 24 * 13) # 6 values per hour per day for 13 parameters
  testthat::expect_equal(nrow(hberg), 6 * 24 * 14) # 6 values per hour per day for 13 parameters
  testthat::expect_equal(sum(is.na(hberg$value)), 204) # counted with excel
  testthat::expect_equal(lubridate::tz(hberg$starttime[1]), "Etc/GMT-1")
  testthat::expect_equal(
    lubridate::with_tz(hberg$starttime[1], "UTC"),
    lubridate::as_datetime("2012-06-20T00:00:00+00:00")
  )

  # this file has missing rows, duplicated, invalid time stamps and most na values but it is still well formed
  incomplete_fn <- system.file("extdata", "IAC-Met_edited.dat", package = "rOstluft.data", mustWork = TRUE)
  incomplete <- read_ethz_iac(incomplete_fn, na.rm = FALSE)
  testthat::expect_equal(nrow(incomplete), 62 * 13) # 62 rows with data
  testthat::expect_equal(sum(is.na(incomplete$value)), 11) # counted with excel

  malformed <- system.file("extdata", "IAC-Met_2010-12-02.dat", package = "rOstluft.data", mustWork = TRUE)
  testthat::expect_error(
    suppressWarnings(read_ethz_iac(malformed))
  )
})
