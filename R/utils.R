#' bind rows of data frames with factors
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Before dplyr 1.0.0 [dplyr::bind_rows()] coerced factors with different levels
#' to characters. This is no longer the case and this function is deprecated
#'
#'
#' _Important_: It is mandatory that all data frames have the same structure
#'
#' @param ... data frames to combine
#'
#' @return combined data frame
#'
#' @export
#' @keywords internal
bind_rows_with_factor_columns <- function(...) {
  lifecycle::deprecate_warn("1.4.1", "bind_rows_with_factor_columns()", "dplyr::bind_rows()")
  # purrr::pmap_df(rlang::list2(...), function(...) {
  #   cols_to_bind <- list(...)
  #   if (all(purrr::map_lgl(cols_to_bind, is.factor))) {
  #     forcats::fct_c(!!!cols_to_bind)
  #   } else {
  #     purrr::invoke(c, .x = cols_to_bind)
  #   }
  # })
  dplyr::bind_rows(...)
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
filter_remove_list <- function(data, filter_list) {

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
filter_keep_list <- function(data, filter_list) {

  create_filter_quo <- function(value, column) {
    value <- ifelse(is.factor(value), as.character(value), value)
    quo(!! sym(column) == !! value)
  }

  filter_arg <- unname(purrr::imap(filter_list, create_filter_quo))
  dplyr::filter(data, !!!filter_arg)
}

#' Cuts / splits data frame in list based on condition
#'
#' @param data data frame to cut
#' @param condition expression used for grouping
#' @param mapping names for list
#'
#' @return list
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data <- cut_on_condition(data, end_interval < endtime, c("TRUE" = "overlaps", "FALSE" = "complete"))
#' }
cut_on_condition <- function(data, condition, mapping) {
  condition <- rlang::enexpr(condition)
  data <- dplyr::group_by(data, condition_ = !!condition)
  keys <- dplyr::group_keys(data)
  data <- dplyr::group_split(data, .keep = FALSE)
  data <- rlang::set_names(data, mapping[as.character(keys$condition_)])

  # with dplyr 1.0 the package vctrs is used and the returned data is list of tbl_df (list<tbl_df[,8]> )
  # this interferes with later usage, so we cast it to a plain list
  as.list(data)
}

quo_as_symbol <- function(quo) {
  rlang::sym(rlang::quo_get_expr(quo))
}

quo_is_character <- function(quo) {
  rlang::is_character(rlang::quo_get_expr(quo))
}

#' Pass grouping definitions into functions
#'
#' Inspired by [dplyr::vars()], but converting strings to symbols and auto names all arguments
#'
#' @param ... Variables to group by. These arguments are automatically
#'   [quoted][rlang::quo] and later [evaluated][rlang::eval_tidy] in the
#'   context of the data frame. They support [unquoting][rlang::quasiquotation].
#'
#' @return named list containing quosures or symbols
#' @keywords internal
vars <- function(...) {
  quos <- as.list(rlang::quos(...))
  quos <- purrr::modify_if(quos, quo_is_character, quo_as_symbol)
  rlang::exprs_auto_name(quos)
}
