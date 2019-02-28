

format_hysplit <- function(tz = "Etc/GMT") {
  r6_format_hysplit$new(tz = tz)
}

r6_format_hysplit <- R6::R6Class(
  "format_hysplit",
  public = list(
    index = NULL,
    value_column = "value",
    serie_columns = c("site"),
    chunk_columns = c("year", "site"),
    chunk_calc = character(),
    unique_columns = c("site", "date", "date2"),
    content_columns = c("year", "site"),
    tz = NULL,
    initialize = function(tz = "Etc/GMT") {
      self$tz <- tz
    },
    sort = function(data, na.rm = TRUE) {
      if (isTRUE(na.rm)) {
        data <- self$na.omit(data)
      }
      dplyr::arrange(data, .data$date, .data$date2)
    },
    merge = function(new_data, old_data) {
      format_merge(new_data, old_data, self$unique_columns)
    },
    na.omit = function(data) {
      data  # has hypslit data even na values?
    },
    chunk_name = function(chunk_data) {
      # better way to do it?
      row <- dplyr::slice(chunk_data, 1)
      self$encode_chunk_name(row$site, row$year)
    },
    chunk_vars = function(chunk_data) {
      # better way to do it?
      row <- dplyr::slice(chunk_data, 1)
      dplyr::select(row, dplyr::one_of(self$chunk_columns))
    },
    encode_chunk_name = function(site, year) {
      fn <- base64url::base64_urlencode(site)
      fs::path_join(c(as.character(year), fn))
    },
    encoded_chunk_names = function(site, year) {
      chunk_names <- tidyr::expand(tibble::tibble(), site, year)
      dplyr::transmute(chunk_names, chunk_name = purrr::pmap(chunk_names, self$encode_chunk_name))
    },
    decode_chunk_name = function(chunk_name) {
      if (is.na(chunk_name)) {
        tibble::tibble(
          chunk_name = character(),
          site = character(),
          year = character()
        )
      } else {
        fn <- tibble::tibble(chunk_name = chunk_name)
        dplyr::mutate(fn, year =  fs::path_dir(.data$chunk_name),
                      site = base64url::base64_urldecode(fs::path_file(.data$chunk_name)))
      }
    }
  )
)
