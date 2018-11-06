context("storage_local_rds")

#TODO smaller test files

# just to be sure, there is nothing before and after the tests
setup({
  path <- rappdirs::user_data_dir(appname = "testthat", appauthor = "rOstluft")
  if (fs::dir_exists(path)) {
    fs::dir_delete(path)
  }
})

teardown({
  path <- rappdirs::user_data_dir(appname = "testthat", appauthor = "rOstluft")
  if (fs::dir_exists(path)) {
    fs::dir_delete(path)
  }
})

test_that("creating store", {
  expect_message(store <- storage_local_rds("testthat",  r6_format_rolf$new(), read.only = F))
})

test_that("put into store", {
  n_staba <- 1219644  # Anzahl Datenunkte in der Datei Zch_Stampfenbachstrasse_2010-2014.csv
  n_ros <- 194182     # Anzahl Datenunkte in der Datei Zch_Rosengartenstrasse_2010-2014.csv
  jahre_staba <- 5    # Anzahl Jahre -> chunks in der Datei  Zch_Stampfenbachstrasse_2010-2014.csv
  jahre_ros <- 2      # Anzahl Jahre -> chunks in der Datei  Zch_Rosengartenstrasse_2010-2014.csv
  staba <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)
  ros <- system.file("extdata", "Zch_Rosengartenstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)
  df <- read_airmo_csv(staba)
  expect_equal(nrow(df), n_staba)

  rolf <- r6_format_rolf$new()
  store_ro <- storage_local_rds("testthat", format = rolf)
  expect_error(store_ro$put(df), class = "ReadOnlyStore")

  store_rw <- storage_local_rds("testthat", format = rolf, read.only = FALSE)
  res <- store_rw$put(df)
  expect_equal(sum(res$n), n_staba)

  chunks <- store_rw$list_chunks()
  expect_equal(nrow(chunks), jahre_staba)

  df <- read_airmo_csv(ros)
  expect_equal(nrow(df), n_ros)

  res <- store_rw$put(df)
  expect_equal(sum(res$n), n_ros)

  chunks <- store_rw$list_chunks()
  expect_equal(nrow(chunks), jahre_staba + jahre_ros)

  content <- store_rw$get_content()
  expect_equal(sum(content$n), n_ros + n_staba)
})

test_that("get from store", {
  rolf <- r6_format_rolf$new()
  store <-  storage_local_rds("testthat", format = rolf)
  co <- store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2010:2018, filter = parameter == "CO")
  expect_equal(nrow(co), 87139)

  staba_2010 <- store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2010)
  expect_equal(nrow(staba_2010), 242962)

  nox_staba_ros_2014 <- store$get(site = c("Zch_Stampfenbachstrasse", "Zch_Rosengartenstrasse"),
                                  interval = "min30", year = 2014, filter = parameter %in% c("NOx", "NO", "NO2"))
  expect_equal(nrow(nox_staba_ros_2014), 52302 + 52245)
})


test_that("destroying store", {
  rolf <- r6_format_rolf$new()
  store_ro <- storage_local_rds("testthat", format = rolf)
  store_rw <- storage_local_rds("testthat", format = rolf, read.only = FALSE)
  expect_warning(store_ro$destroy("DELETE"))
  expect_warning(store_rw$destroy())
  expect_message(store_rw$destroy("DELETE"))
})
