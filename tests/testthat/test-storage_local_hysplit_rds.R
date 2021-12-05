
#TODO smaller test files
#TODO test meta functions

store_name <- "testthat_format_hysplit"
bucket <- "rostluft"

# just to be sure, there is nothing before and after the tests
local_cleanup_storage_s3(store_name, bucket)


test_that("put into store", {
  suppressMessages(store <- storage_local_rds(store_name, format = format_hysplit(), read.only = FALSE))
  p <- system.file("extdata", package = "rOstluft.data", mustWork = TRUE)
  suppressMessages(import_directory(store, p, readRDS, glob = "*hysplit.rds"))

  content <- store$get_content()
  testthat::expect_equal(nrow(content), 4)
  testthat::expect_equal(sum(content$n), 2 * 70810 + 2 * 70183)
})


test_that("get from store", {
  store <- storage_local_rds(store_name, format = format_hysplit())
  res <- store$get(site = c("ZH-Kaserne-hysplit", "SG-BP-hysplit"), year = 2017:2018)
  testthat::expect_equal(nrow(res), 2 * 70810 + 2 * 70183)
})
