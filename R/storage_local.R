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
#' @field meta_path root of all meta files
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
#' `$get_meta(key=NULL)` get meta data. If key is omitted returns all the content of all files in a named list of
#' tibbles, with the file name without extension as name. If key is supplied as argument only the list contains only the
#' specified key.
#'
#' `$put_meta(...)` puts meta data into the store. the name of the argument is used as file name and the value as data.
#'
#' `$destroy(confirmation)` removes all files under path from the file system if "DELETE" is supplied as
#' confirmation
#'
#' @examples
#' ## init store, creates directory if necessary
#' format <- rOstluft::format_rolf()
#' store <- rOstluft::storage_local_rds("example_rOstluft", format, read.only = FALSE)
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
#' ## get list of all chunks
#' store$list_chunks()
#'
#' ## destroy store (careful removes all files on the disk)
#' store$destroy("DELETE")
#'
#' ## missing examples for meta functions
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

#' @param tz time zone for POSIXct's columns. Data is stored in UTC. Converted while reading. It is important, that
#' the input data has the same time zone. Default "Etc/GMT-1"
#'
#' @section storage_local_tsv:
#'
#' This Storage is mainly for debugging purpose or sharing data with another scripting/programming language.
#' _Warning_: Slow and doesn't support logical data type.
#'
#' @return R6 class object of r6_storage_local
#' @export
#' @name r6_storage_local
storage_local_tsv <- function(name, format, path = NULL, read.only = TRUE, tz = "Etc/GMT-1") {

    do_parse <- function(column, guessed, locale) {
    if (guessed %in% c("double", "integer", "number")) {
      readr::parse_number(column, locale = locale)
    } else if (guessed == "datetime") {
      readr::parse_datetime(column, locale = locale)
    } else if (guessed == "date") {
      readr::parse_date(column, locale = locale)
    } else if (guessed == "time") {
      readr::parse_time(column, locale = locale)
    } else {
      as.factor(column)
    }
  }

  tsv_locale <- readr::locale(date_format = "%Y-%m-%dT%H:%M%z",  encoding = "UTF-8", tz = tz)

  read_function <- function(file)  {
    data <- readr::read_tsv(file, col_types = readr::cols(.default = readr::col_character()), locale = tsv_locale)
    spec <- dplyr::summarise_all(data, readr::guess_parser)
    purrr::map2_df(data, spec, do_parse, locale = tsv_locale)
  }

  write_function <- function(object, file) {
    readr::write_tsv(object, file)
  }

  r6_storage_local$new(name, format, path, read.only, "tsv", read_function, write_function)
}


#' @export
r6_storage_local <- R6::R6Class(
  "storage_local",
  public = list(
    format = NULL,
    name = NULL,
    path = NULL,
    data_path = NULL,
    content_path = NULL,
    meta_path = NULL,
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
      self$read_function <- read_function
      self$write_function <- write_function
      self$data_path <- fs::path(path, "data")
      self$content_path <- fs::path(self$path, "content", ext = ext)
      self$meta_path <- fs::path(path, "meta")
      self$read.only <- read.only

      is_new <- !fs::dir_exists(self$path)

      if (read.only && is_new) {
        stop(LocalNotFound(name, self$path))
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

      data <- dplyr::group_by(data, .dots = c(self$format$chunk_calc, self$format$chunk_columns))
      res <- dplyr::do(data, private$merge_chunk(.data))
      dplyr::ungroup(res)
    },
    get = function(filter=NULL, ...) {
      filter <- enquo(filter)
      files <- self$format$get_chunk_names(...)
      files <- dplyr::mutate(files, chunk_path = self$get_chunk_path(.data$chunk_name))
      files <- dplyr::mutate(files, exists = fs::file_exists(.data$chunk_path))
      files <- dplyr::filter(files, .data$exists == TRUE)
      chunks <- purrr::map(files$chunk_path, private$read_chunk, filter = filter)
      purrr::invoke(bind_rows_with_factor_columns, .x = chunks)
    },
    get_chunk_path = function(chunk_name) {
      fs::path(self$data_path, chunk_name, ext = self$ext)
    },
    list_chunks = function() {
      chunk_paths <- fs::dir_ls(self$data_path, recursive = TRUE, type = "file")
      chunk_names <- fs::path_ext_remove(fs::path_rel(chunk_paths, self$data_path))

      if (NROW(chunk_paths) == 0) {
        chunk_vars <- self$format$decode_chunk_name(NA)
      } else {
        chunk_vars <- purrr::map_df(chunk_names, self$format$decode_chunk_name)
      }
      dplyr::bind_cols(chunk_vars, tibble::tibble(chunk_path = chunk_paths))
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
    get_meta = function(key = NULL) {
      if (rlang::is_null(key)) {
        meta_files <- fs::dir_ls(self$meta_path)
        if (NROW(meta_files) > 0) {
          meta_names <- fs::path_ext_remove(fs::path_rel(meta_files, self$meta_path))
          res <- purrr::map(meta_files, self$read_function)
          res <- rlang::set_names(res, meta_names)
        } else {
          res <- rlang::set_names(list())
        }
      } else {
        local.path <- fs::path(self$meta_path, key, ext = self$ext)
        if (fs::file_exists(local.path)) {
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
        args <- dplyr::mutate(args, local.path = fs::path(self$meta_path, .data$name, ext = self$ext))
        purrr::map2(args$value, args$local.path, self$write_function)
      }
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
        new_content <- format_merge(new_content, old_content, self$format$content_columns)
      }
      self$write_function(new_content, self$content_path)
      new_content
    },
    merge_chunk = function(data) {
      # remove calculated columns
      new_data <- dplyr::select(data, -dplyr::one_of(names(self$format$chunk_calc)))
      chunk_name <- self$format$chunk_name(new_data)
      chunk_path <- self$get_chunk_path(chunk_name)

      if (fs::file_exists(chunk_path)) {
        chunk_data <- self$read_function(chunk_path)
        chunk_data <- self$format$merge(new_data, chunk_data)
      } else {
        fs::dir_create(fs::path_dir(chunk_path))
        chunk_data <- new_data
      }
      chunk_data <- self$format$sort(chunk_data)
      self$write_function(droplevels(chunk_data), chunk_path)
      chunk_content <- dplyr::count(chunk_data, .dots = c(self$format$chunk_calc, self$format$serie_columns))
      private$merge_content(chunk_content)

      dplyr::count(data, .dots = c(names(self$format$chunk_calc), self$format$serie_columns))
    }
  )
)
