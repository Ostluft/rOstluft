context("read ethz iac")

test_that("read file", {
  #TODO: add a well formed test file for chn and hberg
  fn <- system.file("extdata", "IAC-Met_2010-12-02.dat",
                     package = "rOstluft.data", mustWork = TRUE)

  testthat::expect_error(
    read_ethz_iac(fn)
  )

})


