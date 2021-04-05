#' Factory to generate conc conversion functions
#'
#' @param parameter to convert
#' @param from start unit
#' @param to target unit
#' @param ... addional parameters passed to conversion function. see [purrr::partial()], [parts_to_mass()],
#'   [mass_to_parts()]
#'
#' @return conversion function with one argument
#' @export
conversion_conc_fun_factory <- function(parameter, from, to, ...)  {
  parameter <- as.character(parameter)  # no surprises with factors

  hit <- dplyr::filter(const$conc_lookup,
    .data$parameter == !!parameter,
    .data$from == !!from,
    .data$to == !!to
  )

  if (nrow(hit) == 1 && !is.null(const$mol_masses[[parameter]])) {
    return(purrr::partial(hit$FUN[[1]], mol_mass = const$mol_masses[[parameter]], ...))
  } else {
    stop(sprintf("No conc conversion function found for parameter %s from %s to %s", parameter, from, to))
  }
}

#' Convert between volume and mass concentrations
#'
#' @description
#' This function converts between volume and mass concentration by standard conditions. The constants used are
#' equivalent to the one used in AIRMO for sites below 1500m a.s.l:
#'
#' ```
#' R   = 8.314151     [kg * m2 / (s2 * mol * K)]
#' T   = 20.00        [°C]
#' p   = 1013.00      [hPa]
#'
#' mol masses:
#' CO  = 28.01        [g / mol]
#' NO  = 30.01        [g / mol]
#' NO2 = 46.01        [g / mol]
#' O3  = 48.00        [g / mol]
#' SO2 = 64.06        [g / mol]
#' ```
#'
#' It is possible to change this constants via functions during run time for a session. See [convert_set_R()],
#' [convert_set_mol_masses()], [convert_set_conc_lookup()]
#'
#'
#' @seealso
#' * [parts_to_mass()]
#' * [mass_to_parts()]
#' * [purrr::partial()]
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
#' convert_conc(airmo_min30, "NO", "\u00b5g/m3", "ppb", method = "return")
#'
convert_conc <- function(data, parameter, from, to, method = "return", ...) {
  FUN <- conversion_conc_fun_factory(parameter, from, to, ...)  # always call the factory to be sure we throw an error
  groups <- cut_conversion_data(data, parameter, from)

  if (rlang::has_name(groups, "convert")) {
    converted <- dplyr::mutate(groups$convert, unit = forcats::as_factor(to),  value = FUN(.data$value))
  } else {
    converted <- data[0, ]
  }

  if (method == "return") {
    return(converted)
  } else if (method == "append") {
    return(dplyr::bind_rows(data, converted))
  } else if (method == "replace") {
    if (!rlang::has_name(groups, "others")) {
      return(converted)
    } else if (!rlang::has_name(groups, "convert")) {
      return(groups$others)
    } else {
      return(dplyr::bind_rows(groups$others, converted))
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
#' @rdname convert_conc
#'
#' @examples
#'
#' conversions <- tibble::tribble(
#'   ~parameter, ~from, ~to,
#'   "CO", "ppm", "mg/m3",
#'   "NO", "ppb", "\u00b5g/m3",
#'   "O3", "ppb", "\u00b5g/m3",
#'   "NO2", "ppb", "\u00b5g/m3",
#'   "SO2", "ppb", "\u00b5g/m3"
#' )
#'
#' min30 <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2013_Jan.csv",
#'                      package = "rOstluft.data", mustWork = TRUE)
#'
#' airmo_min30 <- read_airmo_csv(min30)
#' airmo_min30_parts <- dplyr::filter(airmo_min30, .data$unit == "ppb" | .data$unit == "ppm")
#' convert_conc_multiple(airmo_min30_parts, conversions, method = "return")
convert_conc_multiple <- function(data, conversions, method = "return", ...) {
  if (!tibble::is_tibble(conversions) && !all(c("parameter", "from", "to") %in% names(conversions))) {
    stop("argument conversion must be a tibble and must has the columns parameter, from, to")
  }

  if (method == "return") {
    converted <- purrr::pmap(conversions, convert_conc, data = data, method = "return", ...)
    return(dplyr::bind_rows(!!!converted))
  } else if (method == "append") {
    converted <- purrr::pmap(conversions, convert_conc, data = data, method = "return", ...)
    return(dplyr::bind_rows(data, !!!converted))
  } else if (method == "replace") {
    conversions_list <- purrr::transpose(conversions)
    return(purrr::reduce(conversions_list, apply_convert_conc,  method = "replace", ..., .init = data))
  } else {
    stop(sprintf('Invalid value (%s) for argument method. Should be one of "return", "append", "replace"', method))
  }
}


#' Converts mass to volume concentration
#'
#' @param values mass concentration
#' @param mol_mass in g/mol
#' @param temperature in °C. Default 20 °C
#' @param pressure in hPa. Default 1013.25 hPa
#' @param ... catch additional arguments to keep R happy
#'
#' @return volume concentration
#' @export
mass_to_parts <- function(values, mol_mass, temperature = 20.0, pressure = 1013, ...) {
  # unit conversion: 1 / (g > kg) => * 1000, 1  / (hPa > Pa) => / 100 ==> resulting factor * 10
  values * const$molar_gas_constant * (temperature + 273.15) / pressure / mol_mass * 10
}

#' Converts mass to volume concentration
#'
#' @param values volume concentration
#' @param mol_mass in g/mol
#' @param temperature in °C. Default 20 °C
#' @param pressure in hPa. Default 1013 hPa
#' @param ... catch additional arguments to keep R happy
#'
#' @return mass concentration
#' @export
parts_to_mass <- function(values, mol_mass, temperature = 20.0, pressure = 1013, ...) {
  # unit conversion: (g > kg) => / 1000, (hPa > Pa) => * 100 ==> resulting factor / 10
  values / const$molar_gas_constant / (temperature + 273.15) * pressure * mol_mass / 10
}


#' Helper function to reduce a list of conversions parameters
#'
#' @param data to convert the units
#' @param args the arguments for the conversion. a named list with parameter, to, from as named item
#' @param ...  additional arguments to pass to convert_units
#'
#' @return result of convert_units
#'
#' @keywords internal
apply_convert_conc <- function(data, args, ...) {
  rlang::exec(convert_conc, data, !!!args, ...)
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
  data <- dplyr::group_split(data, .keep = FALSE)
  mapping <- list("TRUE" = "convert", "FALSE" = "others")
  rlang::set_names(data, mapping[as.character(keys$condition)])  # FALSE before TRUE, order don't matters
}

# enviroment containing all user changable values
const <- new.env()

# universal/ideal/molar gas constant R in kg * m2 / (s2 * mol * K)
const$molar_gas_constant <- 8.314151

# in g/mol
const$mol_masses <- tibble::tibble(
  CO = 28.01,
  NO = 30.01,
  NO2 = 46.01,
  O3 = 48.00,
  SO2 = 64.06
)

# lookup table for volume/mass concentration conversions, \u00b5 => µ
const$conc_lookup <- tibble::tribble(
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



#' Customize concentration conversion
#'
#' @description
#' Converting between volume and mass concentrations isn't as straightforward as you may think. The molar gas constant
#' is a natur constant. As such is subject to changes if more accurate measurements are avaible.
#' [CODATA](http://www.codata.org/committees-and-groups/fundamental-physical-constants) is a comittee appointed to
#' frequently publish a collection of natur constants. For the molar gas constant for example:
#' ```
#' CODATA 1986  8.314510
#' CODATA 1998  8.314472
#' CODATA 2010  8.3144621
#' CODATA 2014  8.3144598
#' ```
#'
#' Other questions are the exact values for temperature, pressure and mol masses. This function set allows the
#' customization of all used constants for the conversion. In the example is showed how to use the CODATA 2014 value
#' for the molar gas constant, 1013.25 hPA for the pressure and add Ammoniak (NH3) to the conversions.
#'
#'
#' @seealso
#' * [convert_conc()]
#' * [conversion_conc_fun_factory()]
#' * [rlang::is_bare_double()]
#' * [purrr::partial()]
#'
#' @return The constants for R, mol masses or the lookup table for conversions
#' @export
#'
#' @examples
#' # Update molar mas constant R to CODATA 2014 value
#' convert_set_R(8.3144598)
#'
#' # add NH3 to the existing mol masses
#' mol_masses <- convert_get_mol_masses()
#' mol_masses <- dplyr::bind_cols(mol_masses, "NH3" = 17.03)
#' convert_set_mol_masses(mol_masses)
#'
#' # to update the default pressure rebuild the lookup table
#' # with partial functions of mass_to_parts, parts_to_mass
#' # at the same time add mappings for NH3
#' mass_to_parts_partial <- purrr::partial(mass_to_parts, pressure = 1013.25)
#' parts_to_mass_partial <- purrr::partial(parts_to_mass, pressure = 1013.25)
#'
#' lookup <- tibble::tribble(
#'   ~parameter, ~from, ~to, ~FUN,
#'   "CO", "ppm", "mg/m3", parts_to_mass_partial,
#'   "NO", "ppb", "\u00b5g/m3", parts_to_mass_partial,
#'   "O3", "ppb", "\u00b5g/m3", parts_to_mass_partial,
#'   "NO2", "ppb", "\u00b5g/m3", parts_to_mass_partial,
#'   "SO2", "ppb", "\u00b5g/m3", parts_to_mass_partial,
#'   "CO", "mg/m3", "ppm", mass_to_parts_partial,
#'   "NO", "\u00b5g/m3", "ppb", mass_to_parts_partial,
#'   "O3", "\u00b5g/m3", "ppb", mass_to_parts_partial,
#'   "NO2", "\u00b5g/m3", "ppb", mass_to_parts_partial,
#'   "SO2", "\u00b5g/m3", "ppb", mass_to_parts_partial,
#'   "NH3", "\u00b5g/m3", "ppb", mass_to_parts_partial,
#'   "NH3", "ppb", "\u00b5g/m3", parts_to_mass_partial,
#' )
#'
#' convert_set_conc_lookup(lookup)
#'
convert_get_R <- function() {
  const$molar_gas_constant
}


#' Title
#'
#' @param R new value for molar gas constant in \preformatted{[kg * m2 / (s2 * mol * K)]}
#'
#' @export
#'
#' @rdname convert_get_R
convert_set_R <- function(R) {
  if (!rlang::is_bare_double(R, 1)) {
    stop("R has to be one bare double")
  }

  const$molar_gas_constant <- R
}


#' @export
#'
#' @rdname convert_get_R
convert_get_mol_masses <- function() {
  const$mol_masses
}


#' @param mol_masses A tibble with one row, parameter as name of column and mol mass as bare double value in
#'   \preformatted{[g / mol]}
#'
#' @export
#'
#' @rdname convert_get_R
convert_set_mol_masses <- function(mol_masses) {
  if (!tibble::is_tibble(mol_masses) && nrow(mol_masses) == 1) {
    stop("mol_masses has to be a tibble with one row")
  }

  if (!all(purrr::map_lgl(mol_masses, ~ rlang::is_bare_double(., 1)))) {
    stop("mol_masses should only contain bares doubles")
  }

  const$mol_masses <- mol_masses
}



#' @export
#'
#' @rdname convert_get_R
convert_get_conc_lookup <- function() {
  const$conc_lookup
}


#' @section Lookup Table:
#'
#' The lookup table is used to pick to correct conversion function. The table is a tibble with the columns parameter,
#' from, to and FUN. The [conversion_conc_fun_factory()] uses this lookup table to create a partial function from FUN.
#' The partial function gets the mol mass (as mol_mass) of the parameter and all ... arguments applied as named
#' arguments. This allows to change the default values of [mass_to_parts()] and [parts_to_mass()].
#'
#'
#' @param lookup new lookup table. For Details see section Lookup Table or examples
#'
#' @export
#'
#' @rdname convert_get_R
convert_set_conc_lookup <- function(lookup) {
  if (!tibble::is_tibble(lookup) && !all(tibble::has_name(lookup, c("parameter", "from", "to", "FUN")))) {
    stop("lookup has to be a tibble with the columns parameter, from, to, FUN")
  }

  const$conc_lookup <- lookup
}
