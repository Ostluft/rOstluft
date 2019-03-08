#' apply meta data to rolf data
#'
#' See Tests for examples
#'
#' @section TODO:
#' write one universel function: shouldn't be too hard with nse:
#'   * source and target columns in data (parameter > parameter,  parameter > unit)
#'   * mapping columns in meta for source ( parameter_original > parameter, parameter_original > unit)
#'   * the worst will be the user messages
#'
#' just started the wrong way. Wanted to do both remappings in one go ...
#'
#' @param data input data
#' @param meta meta data
#' @param parameter_orig column in meta for parameter and unit remapping
#' @param parameter source column in meta for replacement of parameter_orig values in data for parameter remapping
#' @param unit source column in meta for replacement of parameter_orig values in data for unit remapping
#' @param mode one of strict, drop, keep, replace
#' @param replacement_parameter named list of replacement values for missing parameter_orig values in meta for
#'   parameter remapping
#' @param replacement_unit named list of replacement values for missing parameter_orig values in meta for
#'   unit remapping
#'
#' @return in rOstluft long format structure
#'
#' @export
#'
rolf_apply_meta <- function(data, meta, parameter_orig, parameter = NULL, unit = NULL, mode = "strict",
                            replacement_parameter = NULL, replacement_unit = NULL) {
  if (!(tibble::is_tibble(data) && tibble::has_name(data, "parameter") && is.factor(data$parameter) &&
        tibble::has_name(data, "unit") && is.factor(data$unit))) {
    stop("data isn't compatible with apply_meta. should be a tibble and has factor columns parameter and unit")
  }

  if (!(tibble::is.tibble(meta) && all(tibble::has_name(meta, c(parameter_orig, parameter, unit))))) {
    stop("meta isn't compatible with apply_meta. should be a tibble and should have the columns defined by parameter_orig, parameter, unit")
  }

  if (!is.null(unit)) {
    mapping <- dplyr::distinct(meta, !!sym(parameter_orig), !!sym(unit))
    mapping <- rlang::set_names(as.character(mapping[[unit]]), mapping[[parameter_orig]])

    needed <- as.character(dplyr::distinct(data, .data$parameter)$parameter)
    available <- needed %in% rlang::names2(mapping)
    missing <- needed[!available]  # names of parameter with missing mapping
    available <- needed[available] # names of parameter with available mapping
    missing_str <- stringr::str_c(missing, collapse = ", ")

    if (length(missing) == 0) {
      data <- dplyr::mutate(data, unit = dplyr::recode(.data$parameter, !!!mapping))
    } else if (mode == "drop") {
      warning(sprintf("dropping original parameters (missing unit mapping): %s", missing_str))
      data <- dplyr::filter(data, .data$parameter %in% available)
      data <- dplyr::mutate(data, unit = dplyr::recode(.data$parameter, !!!mapping))
    } else if (mode == "keep") {
      message(sprintf("keep unit for original parameters (missing unit mapping): %s", missing_str))

      # that is a bit complicated. get the original unit map, take the not not available needed map from
      # the orignal unit map and the available needed from the meta map and combine both
      unit_map <- dplyr::distinct(data, .data$parameter, .data$unit)
      unit_map <- rlang::set_names(as.character(unit_map$unit), unit_map$parameter)
      mapping <- append(unit_map[missing], mapping[available])

      data <- dplyr::mutate(data, unit = dplyr::recode(.data$parameter, !!!mapping))
    } else if (mode == "replace") {
      if (all(missing %in% rlang::names2(replacement_unit))) {
        message(sprintf("replace unit for original parameters (missing unit mapping): %s", missing_str))
        mapping <- append(replacement_unit[missing], mapping[available])
        data <- dplyr::mutate(data, unit = dplyr::recode(.data$parameter, !!!mapping))
      } else {
        stop(sprintf("Replacement unit missing for one of %s", missing_str))
      }
    } else { # mode  == strict and not all(available) => FAIL
      stop(sprintf("missing unit mapping for  original parameters: %s", missing_str))
    }
  }

  if (!is.null(parameter)) {
    mapping <- dplyr::distinct(meta, !!sym(parameter_orig), !!sym(parameter))
    mapping <- rlang::set_names(as.character(mapping[[parameter]]), mapping[[parameter_orig]])

    needed <- as.character(dplyr::distinct(data, .data$parameter)$parameter)
    available <- needed %in% rlang::names2(mapping)
    missing <- needed[!available]  # names of parameter with missing mapping
    available <- needed[available] # names of parameter with available mapping
    missing_str <- stringr::str_c(missing, collapse = ", ")

    if (length(missing) == 0) {
      data <- dplyr::mutate(data, parameter = dplyr::recode(.data$parameter, !!!mapping))
    } else if (mode == "drop") {
      warning(sprintf("dropping original parameters (missing parameter mapping): %s", missing_str))
      data <- dplyr::filter(data, .data$parameter %in% needed[available])
      data <- dplyr::mutate(data, parameter = dplyr::recode(.data$parameter, !!!mapping))
    } else if (mode == "keep") {
      message(sprintf("keep original parameters (missing parameter mapping): %s", missing_str))
      data <- dplyr::mutate(data, parameter = dplyr::recode(.data$parameter, !!!mapping))
    } else if (mode == "replace"){
      if (all(missing %in% rlang::names2(replacement_parameter))) {
        message(sprintf("replace param for original parameters (missing parameter mapping): %s", missing_str))
        mapping <- append(replacement_parameter[missing], mapping[available])
        data <- dplyr::mutate(data, parameter = dplyr::recode(.data$parameter, !!!mapping))
      } else {
        stop(sprintf("Replacement parameter missing for one of %s", missing_str))
      }
    } else { # mode  == strict and not all(available) => FAIL
      stop(sprintf("missing parameter mapping: %s", missing_str))
    }
  }

  data
}
