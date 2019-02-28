context("storage_local_hysplit_rds")

#TODO smaller test files
#TODO test meta functions



store_name <- "testthat_format_hysplit"

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
  store <- storage_local_rds(store_name, format = format_hysplit(), read.only = FALSE)
  p <- system.file("extdata", package = "rOstluft.data", mustWork = TRUE)
  import_directory(store, p, readRDS, glob = "*hysplit.rds")

  content <- store$get_content()
  testthat::expect_equal(nrow(content), 4)
  testthat::expect_equal(sum(content$n), 2 * 70810 + 2 * 70183)
})


test_that("get from store", {
  store <- storage_local_rds(store_name, format = format_hysplit())
  res <- store$get(site = c("ZH-Kaserne-hysplit", "SG-BP-hysplit"), year = 2017:2018)
  testthat::expect_equal(nrow(res), 2 * 70810 + 2 * 70183)
})
