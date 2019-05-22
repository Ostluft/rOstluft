context("storage_local_ps_rds")

#TODO smaller test files
#TODO test meta functions



store_name <- "testthat_format_ps"

# just to be sure, there is nothing before and after the tests
setup({
  path <- rappdirs::user_data_dir(appname = store_name, appauthor = "rOstluft")
  if (fs::dir_exists(path)) {
    fs::dir_delete(path)
  }
})

teardown({
  path <- rappdirs::user_data_dir(appname = store_name, appauthor = "rOstluft")
  if (fs::dir_exists(path)) {
    fs::dir_delete(path)
  }
})


test_that("put into store", {
  store <- storage_local_rds(store_name, format = format_ps(), read.only = FALSE)
  fn <- system.file("extdata", "NO2_PS.rds",  package = "rOstluft.data", mustWork = TRUE)
  data <- readRDS(fn)
  store$put(data)
  content <- store$get_content()
  testthat::expect_equal(nrow(content), 2)
  testthat::expect_equal(sum(content$n), 47)
})


test_that("get from store", {
  store <- storage_local_rds(store_name, format = format_ps())
  res <- store$get(site = "Zch_Stampfenbachstrasse")
  testthat::expect_equal(nrow(res), 23)
})
