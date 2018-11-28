#' https://github.com/cloudyr/aws.signature/ options to authenticate with aws
#'
#'
#'
#' additional constructor options: bucket_name, region, prefix

s3_buckets_exists <- function() {
  aws.s3::bucket_exists("rostluft", region="eu-central-1")
}

s3_bucket_content <- function() {
  aws.s3::get_bucket("rostluft", region="eu-central-1")
}


s3_bucket_content_datastore <- function() {
  aws.s3::get_bucket("rostluft", region="eu-central-1", prefix = "datastore")
}


r6_storage_s3 <- R6::R6Class(
  "storage_s3",
  public = list(
    format = NULL,
    name = NULL,
    bucket = NULL,
    prefix = NULL,
    region = NULL,
    path = NULL,
    data_path = NULL,
    data_s3 = NULL,
    meta_path = NULL,
    meta_s3 = NULL,
    content_path = NULL,
    content_s3 = NULL,
    read.only = TRUE,
    ext = NULL,
    read_function = NULL,
    write_function = NULL,
    verbose = NULL,

    initialize = function(name, format, bucket, prefix, region = NULL, path = NULL, read.only = TRUE,
                          ext = "rds", read_function = readRDS, write_function = saveRDS) {
      if (is.null(path)) {
        path <- rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")
      } else {
        path <- fs::path_abs(path)
      }

      self$name <- name
      self$format <- format
      self$bucket <- bucket
      self$prefix <- prefix
      self$region <- region
      self$path <- path
      self$ext <- ext
      self$read_function <- read_function
      self$write_function <- write_function
      self$data_path <- fs::path(path, "data")
      self$data_s3 <- fs::path(prefix, "data")
      self$content_path <- fs::path(path, "content", ext = ext)
      self$content_s3 <- fs::path(prefix, "content", ext = ext)
      self$meta_path <- fs::path(path, "meta")
      self$meta_s3 <- fs::path(prefix, "meta")
      self$read.only <- read.only

      if (!fs::dir_exists(self$data_path)) {
        fs::dir_create(self$data_path, recursive = TRUE)
        fs::dir_create(self$meta_path, recursive = TRUE)
        message(sprintf("store %s initialized under '%s'", self$name, self$path))
      }

      invisible(self)
    },
    put = function(data) {
      stop("Not yet implemented")
    },
    get = function(filter = NULL, overwrite_cache = FALSE, ...) {
      filter <- enquo(filter)
      files <- self$format$get_chunk_names(...)
      files <- dplyr::mutate(files, chunk_path = self$get_chunk_path(.data$chunk_name))

      files <- dplyr::mutate(files, exists = fs::file_exists(.data$chunk_path))

      if (isTRUE(overwrite_cache)) {
        download <- files
      } else {
        download <- dplyr::filter(files, .data$exists == FALSE)
      }

      purrr::map(download$chunk_name, self$download_chunk)

      files <- dplyr::mutate(files, exists = fs::file_exists(.data$chunk_path))
      files <- dplyr::filter(files, .data$exists == TRUE)

      chunks <- purrr::map(files$chunk_path, private$read_chunk, filter = filter)
      purrr::invoke(bind_rows_with_factor_columns, .x = chunks)
    },
    upload = function() {
      stop("Not yet implemented")
      chunks <- fs::dir_info(self$data_path, recursive = TRUE, type = "file")
      chunks <- dplyr::select(chunks, "path", "modification_time", "change_time","birth_time", "size")
      chunks <- dplyr::mutate(chunks, chunk_rel = fs::path_rel(.data$path, self$data_path))
      chunks <- dplyr::mutate(chunks,
                              chunk_name = fs::path_ext_remove(chunk_rel),
                              chunk_s3 = fs::path(self$data_s3, chunk_rel)
      )
      chunks <- chunks[21:NROW(chunks), ]

      # chunks_s3 <- aws.s3::get_bucket(self$bucket, prefix = self$data_s3, max = Inf)
      # chunks_s3 <- purrr::map_dfr(chunks_s3,  `[`, c("Key", "ETag", "LastModified"))
      # chunks_s3 <- dplyr::bind_rows(purrr::map_dfr(chunks_s3, purrr::flatten))


      put_s3 <- function(path, chunk_s3, ...) {
        aws.s3::put_object(path, chunk_s3, self$bucket, region = self$region, show_progress = TRUE, verbose = TRUE)
      }

      #purrr::pmap(chunks, put_s3)

      aws.s3::put_object(self$content_path, self$content_s3, self$bucket, region = self$region, show_progress = TRUE, verbose = TRUE)
    },
    download_chunk = function(chunk_name) {
      chunk_url <- self$get_chunk_url(chunk_name)
      chunk_path <- self$get_chunk_path(chunk_name)
      if (aws.s3::object_exists(chunk_url, bucket = self$bucket, region = self$region)) {
        aws.s3::save_object(chunk_url, bucket = self$bucket, file = chunk_path, region = self$region)
      }
    },

    get_chunk_path = function(chunk_name) {
      fs::path(self$data_path, chunk_name, ext = self$ext)
    },
    get_chunk_url = function(chunk_name) {
      fs::path(self$data_s3, chunk_name, ext = self$ext)
    },
    merge_chunk = function(chunk_data) {
      stop("Not yet implemented")
    },
    list_chunks = function() {
      chunks_s3 <- aws.s3::get_bucket(self$bucket, prefix = self$data_s3, max = Inf)
      chunks_s3 <- dplyr::bind_rows(purrr::map_dfr(chunks_s3, purrr::flatten))

      # fix etag surrounded by ""
      if (stringi::stri_length(chunks_s3$ETag[[1]]) == 34) {
        chunks_s3 <- dplyr::mutate(chunks_s3, ETag = stringi::stri_sub(.data$ETag, 2, -2))
      }

      chunks_s3 <- dplyr::rename_all(chunks_s3, .funs = dplyr::funs(paste0("s3.", stringi::stri_trans_tolower(.))))
      chunks_s3 <- dplyr::mutate(chunks_s3, chunk_name = fs::path_ext_remove(fs::path_rel(.data$s3.key, self$data_s3)),
                                 s3.size = fs::as_fs_bytes(.data$s3.size))
      chunks_s3 <- dplyr::select(chunks_s3, .data$chunk_name, dplyr::everything())

      chunks_local <- fs::dir_info(self$data_path, recursive = TRUE, type = "file")
      chunks_local <- dplyr::select(chunks_local, "path", "modification_time", "size")
      chunks_local <- dplyr::rename_all(chunks_local, .funs = dplyr::funs(paste0("local.",.)))
      chunks_local <- dplyr::mutate(chunks_local,
                                    chunk_name = fs::path_ext_remove(fs::path_rel(.data$local.path, self$data_path)))


      chunks <- dplyr::full_join(chunks_s3, chunks_local, by="chunk_name")
      chunk_vars <- purrr::map_dfr(chunks$chunk_name, self$format$decode_chunk_name)
      dplyr::bind_cols(chunk_vars, dplyr::select(chunks, -"chunk_name"))
    },
    get_content = function() {
      # TODO caching content?
      if (aws.s3::object_exists(self$content_s3, bucket = self$bucket, region = self$region)) {
        aws.s3::save_object(self$content_s3, bucket = self$bucket, file = self$content_path, region = self$region)
        content <- self$read_function(self$content_path)
      } else {
        content <- NULL
        warning(paste0("No content file available for ", self$name))
      }
      content
    },
    destroy = function(confirmation = "NO") {
      if (confirmation == "DELETE") {
        fs::dir_delete(self$path)
        message(sprintf("Cache for Store %s destroyed", self$name))
      } else {
        warning("Store still alive: wrong confirmation phrase")
      }
    }
  ),
  private = list(
    read_chunk = function(chunk_path, filter) {
      chunk <- self$read_function(chunk_path)
      if (!rlang::quo_is_null(filter)) {
        chunk <-  dplyr::filter(chunk, !!!filter)
      }
      chunk
    }

  )
)



test <- function() {
  format <- format_rolf()
  store <- r6_storage_s3$new("test_import", format, "rostluft", "datastore")
  store$upload()
}

get_store <- function() {
  format <- format_rolf()
  r6_storage_s3$new("test_s3", format, "rostluft", "datastore")
}


