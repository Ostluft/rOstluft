context("storage_s3_rolf_rds")


store_name <- "testthat_s3"
bucket <- "rostluft"

is_s3_admin <- function() {
  return(Sys.getenv("AWS_ADMIN") == "TRUE")
}

destroy_s3_store <- function() {
  if (is_s3_admin()) {
    objects <- aws.s3::get_bucket(bucket, prefix = store_name, max = Inf) # nolint
    objects <- dplyr::bind_rows(purrr::map_dfr(objects, purrr::flatten))
    purrr::map(objects$Key, aws.s3::delete_object, bucket = bucket) # nolint
  }
}


#TODO smaller test files
#TODO tests for meta data handling

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
  destroy_s3_store()
})


test_that("creating store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")
  expect_message(storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name))
})

test_that("put into store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")

  n_staba <- 1219644  # Anzahl Datenunkte in der Datei Zch_Stampfenbachstrasse_2010-2014.csv
  n_ros <- 194182     # Anzahl Datenunkte in der Datei Zch_Rosengartenstrasse_2010-2014.csv
  jahre_staba <- 5    # Anzahl Jahre -> chunks in der Datei  Zch_Stampfenbachstrasse_2010-2014.csv
  jahre_ros <- 2      # Anzahl Jahre -> chunks in der Datei  Zch_Rosengartenstrasse_2010-2014.csv
  staba <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)
  ros <- system.file("extdata", "Zch_Rosengartenstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)
  df <- read_airmo_csv(staba)
  expect_equal(nrow(df), n_staba)

  rolf <- format_rolf()
  store_ro <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name)
  expect_error(store_ro$put(df), class = "ReadOnlyStore")

  store_rw <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name, read.only = FALSE)
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

  testthat::expect_warning(
    store_rw$put(df[0, ])
  )
})

test_that("get from store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")
  rolf <- format_rolf()
  store <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name)
  co <- store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2010:2018, filter = parameter == "CO")
  expect_equal(nrow(co), 87139)

  staba_2010 <- store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2010)
  expect_equal(nrow(staba_2010), 242962)

  nox_staba_ros_2014 <- store$get(site = c("Zch_Stampfenbachstrasse", "Zch_Rosengartenstrasse"),
                                  interval = "min30", year = 2014, filter = parameter %in% c("NOx", "NO", "NO2"))
  expect_equal(nrow(nox_staba_ros_2014), 52302 + 52245)
})

test_that("download store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")
  # delete all local files
  rolf <- format_rolf()
  store_rw <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name, read.only = FALSE)
  store_rw$destroy("DELETE")
  store_rw <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name, read.only = FALSE)
  store_rw$download(year == 2010)
  expect_equal(NROW(fs::dir_ls(store_rw$data_path, recursive = TRUE, type = "file")), 1)
  store_rw$download(site == "Zch_Stampfenbachstrasse")
  expect_equal(NROW(fs::dir_ls(store_rw$data_path, recursive = TRUE, type = "file")), 5)
  store_rw$download()
  expect_equal(NROW(fs::dir_ls(store_rw$data_path, recursive = TRUE, type = "file")), 7)
})

test_that("upload store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")
  # delete all s3 files
  destroy_s3_store()
  store_rw <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name, read.only = FALSE)
  store_rw$upload()
  chunks <- store_rw$list_chunks()
  expect_equal(nrow(dplyr::filter(chunks, is.na(.data$s3.key))), 0)
})


test_that("destroying store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")
  rolf <- format_rolf()
  store_ro <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name)
  store_rw <- storage_s3_rds(store_name,  format_rolf(), bucket, prefix = store_name, read.only = FALSE)
  expect_warning(store_ro$destroy("DELETE"))
  expect_warning(store_rw$destroy())
  expect_message(store_rw$destroy("DELETE"))
})
