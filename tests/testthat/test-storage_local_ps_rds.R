#TODO smaller test files
#TODO test meta functions



store_name <- "testthat_format_ps"

# just to be sure, there is nothing before and after the tests
local_cleanup_storage(store_name)


test_that("put into store", {
  testthat::expect_message(
    store <- storage_local_rds(store_name, format = format_ps(), read.only = FALSE)
  )

  fn <- system.file("extdata", "NO2_PS.rds",  package = "rOstluft.data", mustWork = TRUE)
  data <- readRDS(fn)
  testthat::expect_message(store$put(data))
  content <- store$get_content()
  testthat::expect_equal(nrow(content), 2)
  testthat::expect_equal(sum(content$n), 47)
})


test_that("get from store", {
  store <- storage_local_rds(store_name, format = format_ps())
  res <- store$get(site = "Zch_Stampfenbachstrasse")
  testthat::expect_equal(nrow(res), 23)
})
