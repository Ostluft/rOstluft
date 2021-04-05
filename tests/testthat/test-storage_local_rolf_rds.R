 #TODO smaller test files
#TODO test meta functions

# just to be sure, there is nothing before and after the tests
local_cleanup_storage()

test_that("creating store", {
  expect_error(storage_local_rds("testthat",  format_rolf()), class = "LocalNotFound")
  expect_message(store <- storage_local_rds("testthat",  format_rolf(), read.only = F))
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

  rolf <- format_rolf()
  store_ro <- storage_local_rds("testthat", format = rolf)
  expect_error(store_ro$put(df), class = "ReadOnlyStore")

  store_rw <- storage_local_rds("testthat", format = rolf, read.only = FALSE)

  # expect a message on first put, that column types are saved
  expect_message(res <- store_rw$put(df))

  expect_equal(sum(res$n), n_staba)

  chunks <- store_rw$list_chunks()
  expect_equal(nrow(chunks), jahre_staba)

  df <- read_airmo_csv(ros)
  expect_equal(nrow(df), n_ros)

#  attributes(df$value) = list("a" = "test")

  res <- store_rw$put(df)
  expect_equal(sum(res$n), n_ros)

  chunks <- store_rw$list_chunks()
  expect_equal(nrow(chunks), jahre_staba + jahre_ros)

  content <- store_rw$get_content()
  expect_equal(sum(content$n), n_ros + n_staba)

  expect_warning(store_rw$put(df[0, ]))

  # check handling of incompatible columns
  df <- df[1:10, ]

  # wrong order
  df1 <- dplyr::select(df, value, dplyr::everything())

  # incorrect name
  df2 <- dplyr::select(df, time = starttime, dplyr::everything())

  # incorrect types
  df3 <- dplyr::mutate_if(df, is.factor, as.character)

  expect_error(store_rw$put(df1), class = "IncompatibleColumns")
  expect_error(store_rw$put(df2), class = "IncompatibleColumns")
  expect_error(store_rw$put(df3), class = "IncompatibleColumns")
})

test_that("get from store", {
  rolf <- format_rolf()
  store <-  storage_local_rds("testthat", format = rolf)
  co <- store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2010:2018, filter = parameter == "CO")
  expect_equal(nrow(co), 87139)

  staba_2010 <- store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2010)
  expect_equal(nrow(staba_2010), 242962)

  nox_staba_ros_2014 <- store$get(site = c("Zch_Stampfenbachstrasse", "Zch_Rosengartenstrasse"),
                                  interval = "min30", year = 2014, filter = parameter %in% c("NOx", "NO", "NO2"))
  expect_equal(nrow(nox_staba_ros_2014), 52302 + 52245)
})


test_that("put NA frame", {
  store <-  storage_local_rds("testthat", format_rolf(), read.only = FALSE)
  d1 <- system.file("extdata", "Zch_Stampfenbachstrasse_d1_2013_Jan.csv",
                    package = "rOstluft.data", mustWork = TRUE)
  airmo_d1 <- airmo_d1 <- read_airmo_csv(d1)
  empty <- dplyr::mutate(airmo_d1, value = NA_real_)  # NA is logical => use NA_real_ or we get a incompatible column

  n_content <- nrow(store$get_content())
  n_chunks <- nrow(store$list_chunks())

  res <- store$put(empty)


  testthat::expect_equal(
    nrow(res),
    0
  )

  testthat::expect_equal(
    nrow(store$get_content()),
    n_content
  )

  testthat::expect_equal(
    nrow(store$list_chunks()),
    n_chunks
  )


  store$put(airmo_d1)

  testthat::expect_equal(
    nrow(store$list_chunks()),
    n_chunks + 1
  )

  res <- store$put(empty)

  testthat::expect_equal(
    nrow(store$get_content()),
    n_content
  )

  testthat::expect_equal(
    nrow(store$list_chunks()),
    n_chunks
  )
})


test_that("destroying store", {
  rolf <- format_rolf()
  store_ro <- storage_local_rds("testthat", format = rolf)
  store_rw <- storage_local_rds("testthat", format = rolf, read.only = FALSE)
  expect_warning(store_ro$destroy("DELETE"))
  expect_warning(store_rw$destroy())
  expect_message(store_rw$destroy("DELETE"))
})
