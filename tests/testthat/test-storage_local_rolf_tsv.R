context("storage_local_rolf_tsv")

#TODO smaller test files

store_name <- "testthat_tsv"

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

test_that("creating store", {
  expect_message(store <- storage_local_tsv(store_name,  r6_format_rolf$new(), read.only = F))
})

test_that("put into store", {
  staba <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)
  df <- read_airmo_csv(staba)
  df <- dplyr::filter(df, parameter == "CO")
  rolf <- r6_format_rolf$new()
  store_ro <- storage_local_tsv(store_name, format = rolf)
  expect_error(store_ro$put(df), class = "ReadOnlyStore")

  store_rw <- storage_local_tsv(store_name, format = rolf, read.only = FALSE)
  res <- store_rw$put(df)

})

test_that("get from store", {
  rolf <- r6_format_rolf$new()
  store <-  storage_local_tsv(store_name, format = rolf)
  co <- store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2010:2018, filter = parameter == "CO")
  expect_equal(nrow(co), 87139)
})


test_that("destroying store", {
  rolf <- r6_format_rolf$new()
  store_ro <- storage_local_tsv(store_name, format = rolf)
  store_rw <- storage_local_tsv(store_name, format = rolf, read.only = FALSE)
  expect_warning(store_ro$destroy("DELETE"))
  expect_warning(store_rw$destroy())
  expect_message(store_rw$destroy("DELETE"))
})
