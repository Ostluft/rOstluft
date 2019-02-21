#' bind rows of data frames with factors
#'
#' [dplyr::bind_rows()] coerces factors to characters. This function use the [forcats::fct_c()]
#' to combine the factor columns.
#'
#' _Important_: It is mandatory that all data frames have the same structure
#'
#' @param ... data frames to combine
#'
#' @return combined data frame
#'
#' @export
bind_rows_with_factor_columns <- function(...) {
  purrr::pmap_df(rlang::list2(...), function(...) {
    cols_to_bind <- list(...)
    if (all(purrr::map_lgl(cols_to_bind, is.factor))) {
      forcats::fct_c(!!!cols_to_bind)
    } else {
      purrr::invoke(c, .x = cols_to_bind)
    }
  })
}


#' Get element or default value from Object
#'
#' @param object object from which to get element
#' @param element_name a string matched to the names of the object
#' @param default_value return value if element not found in object
#'
#' @return element or default value
#'
#' @keywords internal
getElement2 <- function(object, element_name, default_value = NULL) {
  if (element_name %in% names(object)){
    object[[element_name]]
  } else {
    default_value
  }
}



#' Remove matching rows
#'
#' Removes all rows with matching all name (=column)/value combination from filter_list.
#'
#' @param data tibble to filter
#' @param filter_list named list, name = column to match
#'
#' @return filtered tibble
#'
#' @keywords internal
filter_remove_list = function(data, filter_list) {

  create_filter_quo <- function(value, column) {
    value <- ifelse(is.factor(value), as.character(value), value)
    quo(!! sym(column) != !! value)
  }

  reduce_filter_quo <- function(acc, nxt) {
    quo(!!acc | !! nxt)
  }

  filter_list_quo <- purrr::imap(filter_list, create_filter_quo)
  filter_arg <- purrr::reduce(filter_list_quo, reduce_filter_quo)
  dplyr::filter(data, !! filter_arg)
}

#' Keep matching rows
#'
#' keep all rows with matching all name (=column)/value combination from filter_list.
#'
#' @param data tibble to filter
#' @param filter_list named list, name = column to match
#'
#' @return filtered tibble
#'
#' @keywords internal
filter_keep_list = function(data, filter_list) {

  create_filter_quo <- function(value, column) {
    value <- ifelse(is.factor(value), as.character(value), value)
    quo(!! sym(column) == !! value)
  }

  filter_arg <- unname(purrr::imap(filter_list, create_filter_quo))
  dplyr::filter(data, !!! filter_arg)
}
