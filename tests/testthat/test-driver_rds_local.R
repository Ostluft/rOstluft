context("driver_rds_local")

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
  expect_message(store <- driver_rds_local("testthat", read.only = FALSE))
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

  store_ro <- driver_rds_local("testthat")
  expect_error(store_ro$put(df), class = "ReadOnlyStore")

  store_rw <- driver_rds_local("testthat", read.only = FALSE)
  res <- store_rw$put(df)
  expect_equal(sum(res$n), n_staba)

  chunks <- store_rw$get_list_of_chunks()
  expect_equal(length(chunks$chunk), jahre_staba)

  df <- read_airmo_csv(ros)
  expect_equal(nrow(df), n_ros)

  res <- store_rw$put(df)
  expect_equal(sum(res$n), n_ros)

  chunks <- store_rw$get_list_of_chunks()
  expect_equal(nrow(chunks), jahre_staba + jahre_ros)

  content <- store_rw$get_content()
  expect_equal(sum(content$n), n_ros + n_staba)
})

test_that("get from store", {
  store <- driver_rds_local("testthat")
  co <- store$get("Zch_Stampfenbachstrasse", "min30", 2010:2018, "CO")
  expect_equal(nrow(co), 87139)

  staba_2010 <- store$get("Zch_Stampfenbachstrasse", "min30", 2010)
  expect_equal(nrow(staba_2010), 242962)

  nox_staba_ros_2014 <- store$get(c("Zch_Stampfenbachstrasse", "Zch_Rosengartenstrasse"),
                                  "min30", 2014, c("NOx", "NO", "NO2"))
  expect_equal(nrow(nox_staba_ros_2014), 52302 + 52245)
})


test_that("destroying store", {
  store_ro <- driver_rds_local("testthat")
  store_rw <- driver_rds_local("testthat", read.only = FALSE)
  expect_warning(store_ro$destroy("DELETE"))
  expect_warning(store_rw$destroy())
  expect_message(store_rw$destroy("DELETE"))
})
