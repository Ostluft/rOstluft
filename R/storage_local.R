#' @title Local storage
#'
#' @description A local storage with flexible file format (default rds). The data format defines the data chunks
#' per file.
#'
#' @field name name of the store
#' @field format data format of the store
#' @field path root of the store
#' @field data_path root of all chunks
#' @field content_path path to the rds file containing statistics of store content
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
#' `$put(data)` puts the data into the store. Stops if store is read only
#'
#' `$get_content()` returns a tibble with the amount of data points per chunk per series
#'
#' `$list_chunks()` get list of all chunks
#'
#' `$destroy(confirmation)` removes all files under path from the file system if "DELETE" is supplied as
#' confirmation
#'
#' @examples
#' ## init store, creates directory if necessary
#' rolf <- rOstluft::format_rolf()
#' store <- rOstluft::storage_local_rds("example_rOstluft", rolf, read.only = FALSE)
#'
#' ## read data from airmo export und put into the store
#' fn <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",
#'                   package = "rOstluft.data", mustWork = TRUE)
#' df <- rOstluft::read_airmo_csv(fn)
#' store$put(df)
#'
#' fn <- system.file("extdata", "Zch_Rosengartenstrasse_2010-2014.csv",
#'                   package = "rOstluft.data", mustWork = TRUE)
#' df <- rOstluft::read_airmo_csv(fn)
#' store$put(df)
#'
#' ## get all data min30 for 2011 and 2012
#' store$get(site="Zch_Stampfenbachstrasse", interval="min30", year=2011:2012)
#'
#' ## get only data for O3
#' store$get(year=2011:2012, site="Zch_Stampfenbachstrasse", interval="min30", filter = parameter == "O3")
#'
#' ## get NOx data from multiple stations
#' store$get(site = c("Zch_Stampfenbachstrasse", "Zch_Rosengartenstrasse"), interval = "min30",
#'           year = 2014, filter = parameter %in% c("NOx", "NO", "NO2"))
#'
#' ## get n data points grouped by intervall, station, parameter, year in the store
#' store$get_content()
#'
#' ## get list of all chunks
#' store$list_chunks()
#'
#' ## destroy store (careful removes all files on the disk)
#' store$destroy("DELETE")
#'
#' @name r6_storage_local
#' @docType class
NULL


#' @param name name of the store
#' @param format data format of the store
#' @param path optional path to create the store under.
#'   Defaults to [rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")][rappdirs::user_data_dir()]
#' @param read.only read only store. disable put, if false and the store doesn't exist, the store will be initiated
#'
#' @return R6 class object of r6_storage_local
#' @export
#' @name r6_storage_local
storage_local_rds <- function(name, format, path = NULL, read.only = TRUE) {
  r6_storage_local$new(name, format, path, read.only)
}

#' @param name name of the store
#' @param format data format of the store
#' @param path optional path to create the store under.
#'   Defaults to [rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")][rappdirs::user_data_dir()]
#' @param read.only read only store. disable put, if false and the store doesn't exist, the store will be initiated
#' @param tz time zone for POSIXct's columns. Data is stored in UTC. Converted while reading. It is important, that
#' the input data has the same time zone. Default "Etc/GMT-1"
#'
#' @return R6 class object of r6_storage_local
#' @export
#' @name r6_storage_local
storage_local_tsv <- function(name, format, path = NULL, read.only = TRUE, tz = "Etc/GMT-1") {
  tsv_locale <- readr::locale(date_format = "%Y-%m-%dT%H:%M%z",  encoding = "UTF-8", tz = tz)

  read_function <- function(file)  {
    readr::read_tsv(file, col_types = readr::cols(), locale = tsv_locale)
  }

  write_function <- function(object, file) {
    readr::write_tsv(object, file)
  }

  r6_storage_local$new(name, format, path, read.only, "tsv", read_function, write_function)
}

#' @export
r6_storage_local <- R6::R6Class(
  'storage_local',
  public = list(
    format = NULL,
    name = NULL,
    path = NULL,
    data_path = NULL,
    content_path = NULL,
    read.only = TRUE,
    ext = NULL,
    read_function = NULL,
    write_function = NULL,

    initialize = function(name, format, path = NULL, read.only = TRUE,
                          ext = "rds", read_function = readRDS, write_function = saveRDS) {
      if (is.null(path)) {
        path <- rappdirs::user_data_dir(appname = name, appauthor = "rOstluft")
      } else {
        path <- fs::path_abs(path)
      }

      self$name <- name
      self$format <- format
      self$path <- path
      self$ext <- ext
      self$read_function = read_function
      self$write_function = write_function
      self$data_path <- fs::path(path, "data")
      self$content_path <- fs::path(self$path, "content", ext = ext)
      self$read.only <- read.only

      is_new <- !fs::dir_exists(self$path)

      if (read.only && is_new) {
        stop(RdsLocalNotFound(name, self$path))
      }

      if (is_new) {
        fs::dir_create(self$data_path, recursive = TRUE)
        fs::dir_create(fs::path(self$path, "input"), recursive = TRUE)
        fs::dir_create(fs::path(self$path, "tmp"), recursive = TRUE)
        fs::dir_create(fs::path(self$path, "log_storr"), recursive = TRUE)

        message(sprintf("Local store %s initialized under '%s'", self$name, self$path))
      }
      invisible(self)
    },
    put = function(data) {
      if (self$read.only) {
        stop(ReadOnlyStore(self$name))
      }

      res <- storage_chunk_nest(data, self)
      res <- dplyr::ungroup(res)
      private$merge_content(res)
      res
    },
    get = function(filter=NULL, ...) {
      filter <- enquo(filter)
      files <- self$format$get_chunk_names(...)
      files <- dplyr::mutate(files, chunk_path = self$get_chunk_path(.data$chunk_name))
      files <- dplyr::mutate(files, exists = fs::file_exists(.data$chunk_path))
      files <- dplyr::filter(files, .data$exists == TRUE)
      chunks <- purrr::map(files$chunk_path, private$read_chunk, filter = filter)
      purrr::invoke(bind_rows_with_factor_columns, .x=chunks)
    },
    get_chunk_path = function(chunk_name) {
      fs::path(self$data_path, chunk_name, ext=self$ext)
    },
    merge_chunk = function(chunk_data) {
      # this function must be public for calls from storage_chunk_grouping
      chunk_name <- self$format$chunk_name(chunk_data)
      chunk_path <- self$get_chunk_path(chunk_name)
      if (fs::file_exists(chunk_path)) {
        dfo <- self$read_function(chunk_path)
        dfn <- self$format$merge(chunk_data, dfo)
      } else {
        fs::dir_create(fs::path_dir(chunk_path))
        dfn <- chunk_data
      }
      dfn <- droplevels(dfn)
      dfn <- self$format$sort(dfn)
      self$write_function(dfn, chunk_path)
      dplyr::count(dfn, .dots=self$format$serie_columns)
    },
    list_chunks = function() {
      chunk_paths <- fs::dir_ls(self$data_path, recursive = TRUE, type = "file")
      chunk_names <- fs::path_rel(chunk_paths, self$data_path)
      chunk_names <- fs::path_ext_remove(chunk_names)
      chunks <- tibble::tibble(chunk_path = chunk_paths, chunk_name = chunk_names)
      chunks <- dplyr::mutate(chunks, chunk_name = purrr::map(.data$chunk_name, self$format$decode_chunk_name))
      tidyr::unnest(chunks)
    },
    get_content = function() {
      if (fs::file_exists(self$content_path)) {
        content <- self$read_function(self$content_path)
      } else {
        content <- NULL
        warning(paste0("Empty Store ", self$name))
      }
      content
    },
    destroy = function(confirmation = "NO") {
      if (confirmation == "DELETE" && self$read.only == FALSE) {
        fs::dir_delete(self$path)
        message(sprintf("Store %s destroyed", self$name))
      } else {
        warning("Store still alive: read.only store or wrong confirmation phrase")
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
    },
    merge_content = function(new_content) {
      if (fs::file_exists(self$content_path)) {
        old_content <- self$read_function(self$content_path)
        str(self$format$conent_columns)
        new_content <- format_merge(old_content, new_content, self$format$content_columns)
      }
      self$write_function(new_content, self$content_path)
      new_content
    }
  )
)
