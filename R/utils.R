#' Merge two data frames in rOstluft long format into one
#'
#' Combines the two data frames. Defaulting to data frame `y`.
#'
#' @param x data.frame in rOstluft long format
#' @param y data.frame in rOstluft long format
#'
#' @return merged data frame
#' @export
merge_rOstluft_longformat <- function(x, y) {
  # for some reason, factors can be coerced to character vectors
  suppressWarnings(
    dfn <- dplyr::full_join(y, x, by = c("startzeit", "station", "parameter", "intervall", "einheit"),
                     suffix = c("", ".old"))
  )
  dfn[["wert"]][is.na(dfn["wert"])] <- dfn[["wert.old"]][is.na(dfn["wert"])]
  dfn <- dplyr::select(dfn, -.data$wert.old)
  dfn <- dplyr::arrange(dfn, .data$startzeit)
  dplyr::mutate_if(dfn, is.character, as.factor)
}



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
  purrr::pmap_df(list(...), function(...) {
    cols_to_bind <- list(...)
    if (all(purrr::map_lgl(cols_to_bind, is.factor))) {
      forcats::fct_c(!!!cols_to_bind)
    } else {
      purrr::invoke(c, .x = cols_to_bind)
    }
  })
}



#' Merge two data frame with store content statistic into one
#'
#' Combines the two data frames. Defaulting to data frame `y`.
#'
#' @param x data frame
#' @param y data frame
#'
#' @return combined data frame
#'
#' @keywords internal
merge_content <- function(x, y) {
  # for some reason, factors can be coerced to character vectors
  suppressWarnings(
    dfn <- dplyr::full_join(y, x, by = c("station", "parameter", "intervall", "einheit", "jahr"),
                            suffix = c("", ".old"))
  )
  dfn[["n"]][is.na(dfn["n"])] <- dfn[["n.old"]][is.na(dfn["n"])]
  dfn <- dplyr::select(dfn, -.data$n.old)
  dplyr::mutate_if(dfn, is.character, as.factor)
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
