pluck_param <- function(data, ...) {
  search <- rlang::ensyms(...)
  dplyr::filter(data, .data$parameter %in% search)
}

pluck_interval <- function(data, ...) {
  search <- rlang::ensyms(...)
  dplyr::filter(data, .data$interval %in% search)
}

pluck_site <- function(data, ...) {
  search <- rlang::ensyms(...)
  dplyr::filter(data, .data$site %in% search)
}

pluck_unit <- function(data, ...) {
  search <- rlang::ensyms(...)
  dplyr::filter(data, .data$unit %in% search)
}

pluck_year <- function(data, ...) {
  # Only strings can be converted to symbols
  search <- purrr::flatten(rlang::list2(...))
  dplyr::filter(data, lubridate::year(.data$starttime) %in% search)
}
