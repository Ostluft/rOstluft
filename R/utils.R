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
  dfs <- list(...)
  purrr::pmap_df(dfs, function(...) {
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
