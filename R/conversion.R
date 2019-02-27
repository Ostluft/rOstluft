

#' Factory to generate conversion functions
#'
#' @param parameter to convert
#' @param from start unit
#' @param to target unit
#' @param ... addional parameters passed to conversion function. see [purrr::partial()], [parts_to_mass()],
#'   [mass_to_parts()]
#'
#' @return conversion function with one argument
#' @export
conversion_fun_factory <- function(parameter, from, to, ...)  {
  hit <- dplyr::filter(convert_conc_lookup, .data$parameter == !! parameter, .data$from == !! from, .data$to == !! to)

  if (nrow(hit) == 1) {
    return(purrr::partial(hit$FUN[[1]], mol_mass = mol_masses[[as.character(parameter)]], ...))
  } else {
    # TODO: consider other means of unit conversions
    # at this point we could incoperate udunits2 and lookup if we find a conversion there
    # https://cran.r-project.org/web/packages/udunits2/index.html
    # the package units is based on udunits2 and in active development
    stop(sprintf("No conversion function found for parameter %s from %s to %s", parameter, from, to))
  }
}

#' Convert the unit of a single parameter
#'
#' @param data containing the parameter to convert
#' @param parameter to convert
#' @param from start unit
#' @param to target unit
#' @param method one of "return", "append", "replace". "return" only returns the converted data, "append" appends the
#'   converted data at the end, "replace" replaces the original data for the parameter with the converted
#' @param ... addional parameters passed to conversion function. see [purrr::partial()], [parts_to_mass()],
#'   [mass_to_parts()]
#'
#' @return converted data
#' @export
#'
#' @examples
#' min30 <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2013_Jan.csv",
#'                      package = "rOstluft.data", mustWork = TRUE)
#'
#' airmo_min30 <- read_airmo_csv(min30)
#' convert_unit(airmo_min30, "NO", "µg/m3", "ppb", method = "return")
#'
convert_unit <- function(data, parameter, from, to, method = "return", ...) {
  FUN <- conversion_fun_factory(parameter, from, to, ...)  # always call the factory to be sure we throw an error
  groups <- cut_conversion_data(data, parameter, from)

  if (!is.null(groups$convert)) {
    converted <- dplyr::mutate(groups$convert, unit = forcats::as_factor(to),  value = FUN(.data$value))
  } else {
    converted <- data[0, ]
  }

  if (method == "return") {
    return(converted)
  } else if (method == "append") {
    return(bind_rows_with_factor_columns(data, converted))
  } else if (method == "replace") {
    if (is.null(groups$others)) {
      return(converted)
    } else if (is.null(groups$convert)) {
      return(groups$others)
    } else {
      return(bind_rows_with_factor_columns(groups$others, converted))
    }
  } else {
    stop(sprintf('Invalid value (%s) for argument method. Should be one of "return", "append", "replace"', method))
  }
}


#' Converts the units of multiple parameters
#'
#' @param conversions tibble with columns paramter, from, to. each row is a conversion
#'
#' @export
#'
#' @rdname convert_unit
#'
#' @examples
#'
#' conversions <- tibble::tribble(
#'   ~parameter, ~from, ~to,
#'   "CO", "ppm", "mg/m3",
#'   "NO", "ppb", "µg/m3",
#'   "O3", "ppb", "µg/m3",
#'   "NO2", "ppb", "µg/m3",
#'   "SO2", "ppb", "µg/m3"
#' )
#'
#' min30 <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2013_Jan.csv",
#'                      package = "rOstluft.data", mustWork = TRUE)
#'
#' airmo_min30 <- read_airmo_csv(min30)
#' airmo_min30_parts <- dplyr::filter(airmo_min30, .data$unit == "ppb" | .data$unit == "ppm")
#' convert_multiple_units(airmo_min30_parts, conversions, method = "return")
convert_multiple_units <- function(data, conversions, method = "return", ...) {
  if (!tibble::is_tibble(conversions) && !all(c("parameter", "from", "to") %in% names(conversions))) {
    stop("argument conversion must be a tibble and must has the columns parameter, from, to")
  }

  if (method == "return") {
    converted <- purrr::pmap(conversions, convert_unit, data = data, method = "return", ...)
    return(bind_rows_with_factor_columns(!!!converted))
  } else if (method == "append") {
    converted <- purrr::pmap(conversions, convert_unit, data = data, method = "return", ...)
    return(bind_rows_with_factor_columns(data, !!!converted))
  } else if (method == "replace") {
    conversions_list <- purrr::transpose(conversions)
    return(purrr::reduce(conversions_list, apply_convert_unit,  method = "replace", ..., .init = data))
  } else {
    stop(sprintf('Invalid value (%s) for argument method. Should be one of "return", "append", "replace"', method))
  }
}


#' Converts mass to volume concentration
#'
#' @param values mass concentration
#' @param mol_mass in kg/mol
#' @param temperature in °C. Default 20 °C
#' @param pressure in hPa. Default 1013.25 hPa
#' @param ... catch additional arguments to keep R happy
#'
#' @return volume concentration
#' @export
mass_to_parts <- function(values, mol_mass, temperature = 20.0, pressure = 1013.25, ...) {
  values * molar_gas_constant * (temperature + 273.15) / pressure / 100 / mol_mass  # / 100 convert hPa to Pa
}

#' Converts mass to volume concentration
#'
#' @param values volume concentration
#' @param mol_mass in kg/mol
#' @param temperature in °C. Default 20 °C
#' @param pressure in hPa. Default 1013.25 hPa
#' @param ... catch additional arguments to keep R happy
#'
#' @return mass concentration
#' @export
parts_to_mass <- function(values, mol_mass, temperature = 20.0, pressure = 1013.25, ...) {
  values / molar_gas_constant / (temperature + 273.15) * pressure * 100 * mol_mass # * 100 convert hPa to Pa
}



# helper function to determinate mol_mass from parts and volume concentrations
# nolint start
# calculate_molmass <- function(mass, parts, temperature = 20.0, pressure = 1013.25) {
#   mass / parts * molar_gas_constant * (temperature + 273.15) / pressure / 100
# }
# nolint end


#' Helper function to reduce a list of conversions parameters
#'
#' @param data to convert the units
#' @param args the arguments for the conversion. a named list with parameter, to, from as named item
#' @param ...  additional arguments to pass to convert_units
#'
#' @return result of convert_units
#'
#' @keywords internal
apply_convert_unit <- function(data, args, ...) {
  rlang::exec(convert_unit, data, !!!args, ...)
}

#' Cut data for conversion
#'
#' This function splits off the parameter for conversion
#'
#' TODO: create an all purpuse function?
#'
#' @param data tibble with input data
#' @param parameter parameter for conversion
#' @param unit from this unit to another
#'
#' @return named list: $convert parameter for conversion, $others everything else
#'
#' @keywords internal
cut_conversion_data <- function(data, parameter, unit) {
  data <- dplyr::group_by(data, condition = .data$parameter == !!parameter & .data$unit == !!unit )
  keys <- dplyr::group_keys(data)
  data <- dplyr::group_split(data, keep = FALSE)
  mapping <- list("TRUE" = "convert", "FALSE" = "others")
  rlang::set_names(data, mapping[as.character(keys$condition)])  # FALSE before TRUE, order don't matters
}


# universal/ideal/molar gas constant R in kg * m2 / (s2 * mol * K)
molar_gas_constant <- 8.3144598

# in kg/mol
mol_masses <- tibble::tibble(
  CO = 0.02800413,
  NO = 0.03000371,
  NO2 = 0.04600035,
  O3 = 0.04798994,
  SO2 = 0.06404657
)



# lookup table for volume/mass concentration conversions, \u00b5 => µ
convert_conc_lookup <- tibble::tribble(
  ~parameter, ~from, ~to, ~FUN,
  "CO", "ppm", "mg/m3", parts_to_mass,
  "NO", "ppb", "\u00b5g/m3", parts_to_mass,
  "O3", "ppb", "\u00b5g/m3", parts_to_mass,
  "NO2", "ppb", "\u00b5g/m3", parts_to_mass,
  "SO2", "ppb", "\u00b5g/m3", parts_to_mass,
  "CO", "mg/m3", "ppm", mass_to_parts,
  "NO", "\u00b5g/m3", "ppb", mass_to_parts,
  "O3", "\u00b5g/m3", "ppb", mass_to_parts,
  "NO2", "\u00b5g/m3", "ppb", mass_to_parts,
  "SO2", "\u00b5g/m3", "ppb", mass_to_parts
)
