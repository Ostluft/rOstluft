#' @title s3 storage
#'
#' @description A s3 storage with flexible file format (default rds). The data format defines the data chunks
#' per file. The data is cached locally. This local cache can be used as local storage. For performance enhancement it
#' is recommended after acquiring all needed data from s3 to use the cache as local storage.
#'
#' @field name name of the store
#' @field format data format of the store
#' @field bucket s3 bucket containing the store
#' @field region aws region of the bucket
#' @field prefix of the s3 object keys. see [Object Key and Metadata](https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html)
#' @field path local root of the store
#' @field data_path local root of all chunks
#' @field data_s3 s3 root key of all chunks
#' @field content_path local path to the rds file containing statistics of store content
#' @field content_s3 s3 object key to the rds file containing statistics of store content
#' @field meta_path local root of all meta data files
#' @field meta_s3 s3 root key of all meta data files
#' @field read.only flag for read.only usage of store. Default TRUE
#' @field ext file extension for chunks. Default "rds"
#' @field read_function function(file) for reading chunks from disk. Default [base::readRDS()]
#' @field write_function function(object, file) for writing chunks to disk. Default [base::saveRDS()]
#'
#' @section Methods:
#'
#' `$get(filter=NULL, ...)` get data from the store. The name of the arguments depend on the format. The filter
#'   argument is applied to each chunk.
#'
#' `$download(...)` downloads data from s3 to the local cache. The dots arguments are used to filter the output of
#'  list_chunks(). Only the matching rows will be downloaded.
#'
#' `$put(data)` puts the data into the store. Stops if store is read only
#'
#' `$upload()` uploads content, meta data and all new and changed chunks to the s3 storage. For big additions to the
#' store the recommend way is to use a local storage to modify the cache and use this function to apply the change.
#'
#' `$get_content()` returns a tibble with the amount of data points per chunk per series
#'
#' `$list_chunks()` get list of all chunks in s3 and local
#'
#' `$get_meta(key=NULL)` get meta data. If key is omitted returns all the content of all files in a named list of
#' tibbles, with the file name without extension as name. If key is supplied as argument only the list contains only the
#' specified key.
#'
#' `$put_meta(...)` puts meta data into the store. the name of the argument is used as file name and the value as data.
#'
#' `$get_local_storage()` returns a storage to work with the cached data like a local storage
#'
#' `$destroy(confirmation)` removes all files under path from the file system if "DELETE" is supplied as
#' confirmation
#'
#' @section Authentication:
#'
#' See the documentation of [aws.signature](https://github.com/cloudyr/aws.signature/) for ways to provide the
#' necessary informations. The simplest way is to use environment variables defined in a `.Renviron` file in the
#' root directory of a RStudio Project:
#'
#' ```
#' AWS_ACCESS_KEY_ID = "xxxxxxxxxxxxxxxxxx"
#' AWS_SECRET_ACCESS_KEY = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#' AWS_DEFAULT_REGION = "eu-central-1"
#' ```
#'
#'
#' @examples
#' ## init store, creates directory if necessary
#' format <- rOstluft::format_rolf()
#' store <- rOstluft::storage_s3_rds("ol_esd", format, "rostluft", read.only = FALSE)
#'
#' ## get all data min30 for 2011 and 2012
#' store$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2011:2012)
#'
#' ## get only data for O3
#' store$get(year = 2011:2012, site = "Zch_Stampfenbachstrasse", interval = "min30",
#'           filter = parameter == "O3")
#'
#' ## get NOx data from multiple stations
#' store$get(site = c("Zch_Stampfenbachstrasse", "Zch_Rosengartenstrasse"), interval = "min30",
#'           year = 2014, filter = parameter %in% c("NOx", "NO", "NO2"))
#'
#' ## get n data points grouped by intervall, station, parameter, year in the store
#' store$get_content()
#'
#' ## get list of all chunks, show only local files
#' dplyr::filter(store$list_chunks(), !is.na(local.path))
#'
#' ## download all data for site Zch_Rosengartenstrasse before 2016
#' store$download(site == "Zch_Rosengartenstrasse", year < 2016)
#'
#' ## now there should be 2 more local files
#' dplyr::filter(store$list_chunks(), !is.na(local.path))
#'
#' ## get all meta data
#' store$get_meta()
#'
#' ## or a specific meta file
#' store$get_meta("meta_ostluft")
#'
#' ## get the cache as local storage
#' local <- store$get_local_storage()
#' local$list_chunks()
#'
#' ## get cached data
#' local$get(site = "Zch_Stampfenbachstrasse", interval = "min30", year = 2011:2012)
#'
#' ## destroy store (careful removes all files on the disk)
#' store$destroy("DELETE")
#'
#' ## No examples for write operations
#'
#' @name r6_storage_s3
#' @docType class
NULL



#' Title
#' @param name name of the store
#' @param format data format of the store
#' @param bucket name in aws s3
#' @param prefix in aws s3
#' @param region aws region
#' @param read.only read only store. disables put, upload
#'
#' @return R6 class object of storage_s3
#' @export
#' @name r6_storage_s3
storage_s3_rds <- function(name, format, bucket, prefix = NULL, region = NULL, read.only = TRUE) {
  r6_storage_s3$new(name, format, bucket, prefix = prefix, region = region, read.only = read.only)
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

    initialize = function(name, format, bucket, prefix = NULL, region = NULL, path = NULL, read.only = TRUE,
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
      self$data_s3 <- ifelse(is.null(prefix), "data", fs::path(prefix, "data"))
      self$content_path <- fs::path(path, "content", ext = ext)
      self$content_s3 <- ifelse(is.null(prefix), fs::path("content", ext = ext), fs::path(prefix, "content", ext = ext))
      self$meta_path <- fs::path(path, "meta")
      self$meta_s3 <- ifelse(is.null(prefix), "meta", fs::path(prefix, "meta"))
      self$read.only <- read.only

      is_new <- !fs::dir_exists(self$path)

      if (is_new) {
        fs::dir_create(self$data_path, recursive = TRUE)
        fs::dir_create(self$meta_path, recursive = TRUE)
        message(sprintf("store %s initialized under '%s'", self$name, self$path))
      }

      invisible(self)
    },
    put = function(data) {
      if (self$read.only) {
        stop(ReadOnlyStore(self$name))
      }

      data <- dplyr::group_by(data, .dots = c(self$format$chunk_calc, self$format$chunk_columns))
      res <- dplyr::do(data, private$merge_chunk(.data))
      dplyr::ungroup(res)
    },
    get = function(filter = NULL, ...) {
      filter <- rlang::enquo(filter)
      files <- self$format$get_chunk_names(...)
      files <- dplyr::mutate(files, chunk_path = self$get_chunk_path(.data$chunk_name))
      purrr::map(files$chunk_name, self$download_chunk)
      files <- dplyr::filter(files, fs::file_exists(.data$chunk_path))
      chunks <- purrr::map(files$chunk_path, private$read_chunk, filter = filter)
      purrr::invoke(bind_rows_with_factor_columns, .x = chunks)
    },
    download = function(...) {
      self$get_content()
      self$get_meta()

      chunks <- self$list_chunks()
      chunks <- dplyr::filter(chunks, ...)

      missing_files <- dplyr::filter(chunks, is.na(.data$local.path))
      missing_files <- dplyr::mutate(missing_files, local.path = self$get_chunk_path(.data$chunk_name))

      changed_files <- dplyr::filter(chunks, !is.na(.data$local.path))
      changed_files$local.md5 <- purrr::map_chr(changed_files$local.path, digest::digest, algo ="md5", file = TRUE)
      changed_files <- dplyr::filter(changed_files, .data$local.md5 != .data$s3.etag)

      purrr::map2(missing_files$s3.key, missing_files$local.path, private$s3_save)
      purrr::map2(changed_files$s3.key, changed_files$local.path, private$s3_save)
    },
    upload = function() {
      if (self$read.only) {
        stop(ReadOnlyStore(self$name))
      }

      # upload modified data files
      chunks <- self$list_chunks()

      # local files missing in s3 -> need to calculate s3.key
      missing_files <- dplyr::filter(chunks, is.na(.data$s3.key))
      missing_files <- dplyr::mutate(missing_files, s3.key = self$get_chunk_url(.data$chunk_name))

      # find changed files (s3.etag != local.md5 hash)
      changed_files <- dplyr::filter(chunks, !is.na(.data$local.path), !is.na(.data$s3.key))
      changed_files$local.md5 <- purrr::map_chr(changed_files$local.path, digest::digest, algo ="md5", file = TRUE)
      changed_files <- dplyr::filter(changed_files, .data$local.md5 != .data$s3.etag)

      # upload missing and changed files
      purrr::map2(missing_files$local.path, missing_files$s3.key, private$s3_put)
      purrr::map2(changed_files$local.path, changed_files$s3.key, private$s3_put)

      # upload content file
      if (fs::file_exists(self$content_path)) {
        aws.s3::put_object(self$content_path, self$content_s3, self$bucket, region = self$region)
      }

      # upload meta data
      meta_local <- tibble::tibble(local.path = fs::dir_ls(self$meta_path, recursive = TRUE, type = "file"))
      meta_local <- dplyr::mutate(meta_local, name = fs::path_rel(.data$local.path, self$meta_path))
      meta_local <- dplyr::mutate(meta_local, s3.key = fs::path(self$meta_s3, .data$name))
      purrr::map2(meta_local$local.path, meta_local$s3.key, private$s3_put)
    },
    download_chunk = function(chunk_name) {
      chunk_url <- self$get_chunk_url(chunk_name)
      chunk_path <- self$get_chunk_path(chunk_name)
      private$download_file(chunk_url, chunk_path)
    },
    get_chunk_path = function(chunk_name) {
      fs::path(self$data_path, chunk_name, ext = self$ext)
    },
    get_chunk_url = function(chunk_name) {
      fs::path(self$data_s3, chunk_name, ext = self$ext)
    },
    list_chunks = function() {
      #TODO cache list_chunks?
      chunks_s3 <- s3_list_objects(self$bucket,self$data_s3, Inf, fixEtag = TRUE, remove_folders = TRUE)
      chunks_s3 <- dplyr::rename_all(chunks_s3, .funs = dplyr::funs(paste0("s3.", stringi::stri_trans_tolower(.))))
      chunks_s3 <- dplyr::mutate(chunks_s3, chunk_name = fs::path_ext_remove(fs::path_rel(.data$s3.key, self$data_s3)),
                                 s3.size = fs::as_fs_bytes(.data$s3.size))
      chunks_s3 <- dplyr::select(chunks_s3, "chunk_name", "s3.key", "s3.lastmodified", "s3.etag", "s3.size")

      chunks_local <- fs::dir_info(self$data_path, recursive = TRUE, type = "file")
      chunks_local <- dplyr::select(chunks_local, "path", "modification_time", "size")
      chunks_local <- dplyr::rename_all(chunks_local, .funs = dplyr::funs(paste0("local.",.)))
      chunks_local <- dplyr::mutate(chunks_local,
                                    chunk_name = fs::path_ext_remove(fs::path_rel(.data$local.path, self$data_path)))
      chunks <- dplyr::full_join(chunks_s3, chunks_local, by="chunk_name")

      if (nrow(chunks) == 0) {
        chunk_vars <- self$format$decode_chunk_name(NA)
      } else {
        chunk_vars <- purrr::map_dfr(chunks$chunk_name, self$format$decode_chunk_name)
      }
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
    get_local_storage = function() {
      r6_storage_local$new(
        name = self$name, format = self$format, path = self$path, read.only = self$read.only,
        ext = self$ext, read_function = self$read_function, write_function = self$write_function
      )
    },
    get_meta = function(key = NULL) {
      if (rlang::is_null(key)) {
        meta_s3 <- s3_list_objects(self$bucket, prefix = self$meta_s3, max = Inf)
        meta_s3 <- dplyr::mutate(meta_s3, path = fs::path_rel(.data$Key, self$meta_s3))
        meta_s3 <- dplyr::mutate(meta_s3, name = fs::path_ext_remove(.data$path),
                                 local.path = fs::path(self$meta_path, .data$path))
        purrr::map2(meta_s3$Key, meta_s3$local.path, private$s3_save)
        res <- purrr::map(meta_s3$local.path, self$read_function)
        res <- rlang::set_names(res, meta_s3$name)
      } else {
        s3.key <- fs::path(self$meta_s3, key, ext = self$ext)
        local.path <- fs::path(self$meta_path, key, ext = self$ext)
        if (isTRUE(private$download_file(s3.key, local.path))) {
          res <- list()
          res[[key]] <- self$read_function(local.path)
        } else {
          stop(MetaKeyNotFound(self$name, key))
        }
      }
      res
    },
    put_meta = function(...) {
      if (self$read.only) {
        stop(ReadOnlyStore(self$name))
      }

      args <- rlang::dots_list(...)
      if (NROW(args) > 0) {
        args <- tibble::enframe(args)
        args <- dplyr::mutate(args, local.path = fs::path(self$meta_path, .data$name, ext = self$ext),
                              s3.key = fs::path(self$meta_s3, .data$name, ext = self$ext))
        purrr::map2(args$value, args$local.path, self$write_function)
        purrr::map2(args$local.path, args$s3.key, private$s3_put)
      }
    },
    destroy = function(confirmation = "NO") {
      if (confirmation == "DELETE" && self$read.only == FALSE) {
        fs::dir_delete(self$path)
        message(sprintf("Cache for Store %s destroyed", self$name))
      } else {
        warning("Store still alive: read.only store or wrong confirmation phrase")
      }
    }
  ),
  private = list(
    # helper functions around aws.s3 functions to use with purrr
    s3_save = function(s3.key, local.path) {
      aws.s3::save_object(s3.key, file = local.path, bucket = self$bucket, region = self$region)
    },
    s3_put = function(local.path, s3.key) {
      aws.s3::put_object(local.path, s3.key, self$bucket, region = self$region)
    },
    # check existance of object in s3 and compare with local version to save bandwidth
    download_file = function(s3.key, local.path) {
      resp <- aws.s3::head_object(s3.key, bucket = self$bucket, region = self$region)
      if (isTRUE(resp)) {
        etag <- s3_fixEtag(attr(resp, "etag"))
        # only download files not existing local or different md5 hashes
        if (!fs::file_exists(local.path) || etag != digest::digest(local.path, algo="md5", file = TRUE)) {
          aws.s3::save_object(s3.key, file = local.path, bucket = self$bucket, region = self$region)
        }
      }
      resp
    },
    read_chunk = function(chunk_path, filter) {
      chunk <- self$read_function(chunk_path)
      if (!rlang::quo_is_null(filter)) {
        chunk <-  dplyr::filter(chunk, !!!filter)
      }
      chunk
    },
    merge_chunk = function(data) {
      # remove calculated columns
      new_data <- dplyr::select(data, -dplyr::one_of(names(self$format$chunk_calc)))
      chunk_name <- self$format$chunk_name(new_data)
      chunk_path <- self$get_chunk_path(chunk_name)
      chunk_url <- self$get_chunk_url(chunk_name)

      if (self$download_chunk(chunk_name)) {
        chunk_data <- self$read_function(chunk_path)
        chunk_data <- self$format$merge(new_data, chunk_data)
      } else {
        fs::dir_create(fs::path_dir(chunk_path))
        chunk_data <- new_data
      }
      chunk_data <- self$format$sort(chunk_data)
      self$write_function(droplevels(chunk_data), chunk_path)
      aws.s3::put_object(chunk_path, chunk_url, self$bucket, region = self$region)

      chunk_content <- dplyr::count(chunk_data, .dots = c(self$format$chunk_calc, self$format$serie_columns))
      private$merge_content(chunk_content)

      dplyr::count(data, .dots = c(names(self$format$chunk_calc), self$format$serie_columns))
    },
    merge_content = function(new_content) {
      if (private$download_file(self$content_s3, self$content_path)) {
        old_content <- self$read_function(self$content_path)
        new_content <- format_merge(new_content, old_content, self$format$content_columns)
      }
      self$write_function(new_content, self$content_path)
      aws.s3::put_object(self$content_path, self$content_s3, self$bucket, region = self$region)
      new_content
    }
  )
)

s3_fixEtag <- function(etag) {
  # fix etag surrounded by ""
  if (stringi::stri_length(dplyr::first(etag)) == 34) {
    stringi::stri_sub(etag, 2, -2)
  } else {
    etag
  }
}

s3_list_objects <- function(bucket, prefix = NULL, max = NULL, marker = NULL, fixEtag = FALSE, remove_folders = TRUE) {
  objects <- aws.s3::get_bucket(bucket, prefix = prefix, max = max, marker = marker)
  objects <- purrr::map_df(objects, purrr::flatten)

  if (nrow(objects) == 0) {
    objects <- tibble::tibble(
      Key = character(),
      LastModified = character(),
      ETag = character(),
      Size = double(),
      Owner = character(),
      StorageClass = character(),
      Bucket = character()
    )
  }

  if (rlang::is_true(remove_folders) && nrow(objects) > 0) {
    objects <- dplyr::filter(objects, .data$Size > 0)  # remove all folders
  }

  if (rlang::is_true(fixEtag) && nrow(objects) > 0) {
    objects <- dplyr::mutate(objects, ETag = s3_fixEtag(.data$ETag))
  }
  objects
}
