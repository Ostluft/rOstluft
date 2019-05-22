context("storage_s3_hysplit_rds")


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


test_that("put into store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")
  store <- storage_s3_rds(store_name, format_hysplit(), bucket, prefix = store_name,  read.only = FALSE)
  p <- system.file("extdata", package = "rOstluft.data", mustWork = TRUE)
  import_directory(store, p, readRDS, glob = "*hysplit.rds")

  content <- store$get_content()
  testthat::expect_equal(nrow(content), 4)
  testthat::expect_equal(sum(content$n), 2 * 70810 + 2 * 70183)
})


test_that("get from store", {
  testthat::skip_if_not(is_s3_admin(), "Skip Test: no administator rights")
  store <- storage_s3_rds(store_name, format_hysplit(), bucket, prefix = store_name)

  data_files <- fs::dir_ls(store$data_path, recurse = TRUE, type = "file")

  fs::file_delete(data_files[1])
  fs::file_delete(data_files[3])

  res <- store$get(site = c("ZH-Kaserne-hysplit", "SG-BP-hysplit"), year = 2017:2018)
  testthat::expect_equal(nrow(res), 2 * 70810 + 2 * 70183)
})
