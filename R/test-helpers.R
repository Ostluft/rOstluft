local_cleanup_storage <- function(store_name = "testthat", env = parent.frame()) {
  path <- rappdirs::user_data_dir(appname = store_name, appauthor = "rOstluft")
  delete_store <- function () {
    if (fs::dir_exists(path)) {
      fs::dir_delete(path)
    }
  }

  delete_store()
  withr::defer(delete_store(), envir = env)
}

local_cleanup_storage_s3 <- function(
  store_name = "testthat",
  bucket = "rostluft",
  env = parent.frame()
) {
  local_cleanup_storage(store_name, env)

  destroy_s3_store <- function() {
    objects <- aws.s3::get_bucket(bucket, prefix = store_name, max = Inf) # nolint
    if (length(objects) > 0) {
      objects <- dplyr::bind_rows(purrr::map_dfr(objects, purrr::flatten))
      purrr::map(objects$Key, aws.s3::delete_object, bucket = bucket) # nolint
    }
  }

  if (Sys.getenv("AWS_ADMIN") == "TRUE") {
    destroy_s3_store()
    withr::defer(destroy_s3_store(), envir = env)
  }

}
