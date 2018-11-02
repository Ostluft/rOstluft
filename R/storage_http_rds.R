r6_storage_http_rds <- R6::R6Class(
  'storage_http_rds',
  public = list(
    format = NULL,
    name = NULL,
    url = NULL,
    path = NULL,
    rds_path = NULL,
    rds_content_path = NULL,
    rds_content_url = NULL,
    read.only = TRUE,

    initialize = function(name, format, url, path = NULL) {
      if (is.null(path)) {
        path <- rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")
      } else {
        path <- fs::path_abs(path)
      }

      self$format <- format
      self$name <- name
      self$url <- url
      self$path <- path
      self$rds_path <- fs::path(path, "rds")
      self$rds_content_path <- fs::path(path, "content.rds")
      self$rds_content_url <- fs::path(url, "content.rds")


      if (!fs::dir_exists(self$rds_path)) {
        fs::dir_create(self$rds_path, recursive = TRUE)
        message(sprintf("Cache for http rds store %s initialized under '%s'", self$name, self$path))
      }
      invisible(self)
    },
    put = function(data) {
      stop(ReadOnlyStore(self$name))
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
      purrr::invoke(bind_rows_with_factor_columns, .x=chunks)
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
      fs::path(self$rds_path, chunk_name, ext="rds")
    },
    get_chunk_url = function(chunk_name) {
      fs::path(self$url, chunk_name, ext="rds")
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


test <- function(data) {
  rolf <- r6_format_rolf$new()
  rds_http <- r6_storage_http_rds$new("zueriluft", rolf, "https://zueriluft.ch/tools/rds/")
  #rds_http$get(interval = "min30", site="Zch_Stampfenbachstrasse", year=2012:2018, filter = parameter %in% c("CO", "O3" ) & value > 15 )
  rds_http$get_content()
}
