# TODO R6 Base Class for Storage?


#' Chunks and store data with additionals calculated
#'
#' @param data for chunking
#' @param storage to store data into
#'
#' @return tibble with n = count of data points per chunk per series
#' @keywords internal
storage_chunk_nest <- function(data, storage) {
  df <- dplyr::group_by(data, .dots = storage$format$chunk_calc)
  df <- tidyr::nest(df, .key = groups)
  df <- dplyr::mutate(df, groups = purrr::map(.data$groups, storage_chunk_grouping, storage))
  tidyr::unnest(df, groups)
}

#' Chunks and store data
#'
#' @param data for chunking
#' @param storage to store data into
#'
#' @return tibble with n = count of data points per chunk per series
#' @keywords internal
storage_chunk_grouping <- function(groups, storage) {
  groups <- dplyr::group_by(groups, .dots=storage$format$chunk_columns)
  groups <- dplyr::do(groups, storage$merge_chunk(.data))
  dplyr::ungroup(groups)
}
