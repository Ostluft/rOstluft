r6_storage_http <- R6::R6Class(
  "storage_http",
  public = list(
    format = NULL,
    name = NULL,
    url = NULL,
    path = NULL,
    data_path = NULL,
    content_path = NULL,
    read.only = TRUE,
    ext = NULL,
    read_function = NULL,
    write_function = NULL,

    initialize = function(name, format, url, path = NULL, read.only = TRUE,
                          ext = "rds", read_function = readRDS, write_function = saveRDS) {
      if (is.null(path)) {
        path <- rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")
      } else {
        path <- fs::path_abs(path)
      }

      self$name <- name
      self$format <- format
      self$url <- url
      self$path <- path
      self$ext <- ext
      self$read_function <- read_function
      self$write_function <- write_function
      self$data_path <- fs::path(path, "data")
      self$content_path <- fs::path(self$path, "content", ext = ext)
      self$read.only <- read.only

      if (!fs::dir_exists(self$data_path)) {
        fs::dir_create(self$data_path, recursive = TRUE)
        message(sprintf("Cache for http rds store %s initialized under '%s'", self$name, self$path))
      }
      invisible(self)
    },
    put = function(data) {
      stop(ReadOnlyStore(self$name))
    },
    get = function(filter = NULL, overwrite_cache = FALSE, ...) {
      filter <- enquo(filter)
      files <- self$format$encoded_chunk_names(...)
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
    download_chunk = function(chunk_name) {
      chunk_url <- self$get_chunk_url(chunk_name)
      chunk_path <- self$get_chunk_path(chunk_name)
      resp <- httr::HEAD(chunk_url)
      if (httr::status_code(resp) == 200) {
        httr::GET(chunk_url, httr::write_disk(chunk_path, overwrite = TRUE))
      }
    },
    get_chunk_path = function(chunk_name) {
      fs::path(self$data_path, chunk_name, ext = self$ext)
    },
    get_chunk_url = function(chunk_name) {
      fs::path(self$url, chunk_name, ext = self$ext)
    },
    merge_chunk = function(chunk_data) {
      stop(ReadOnlyStore(self$name))
    },
    list_chunks = function() {
      stop("Not Supported by this storage")
    },
    get_content = function() {
      resp <- httr::HEAD(self$rds_content_url)
      if (httr::status_code(resp) == 200) {
        httr::GET(self$rds_content_url, httr::write_disk(self$rds_content_path, overwrite = TRUE))
        content <- readRDS(self$rds_content_path)
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
      chunk <- readRDS(chunk_path)
      if (!rlang::quo_is_null(filter)) {
        chunk <-  dplyr::filter(chunk, !!!filter)
      }
      chunk
    }
  )
)
