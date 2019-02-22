molar_gas_constant <- 8.3144598

# in kg/mol
mol_masses <- tibble::tibble(
  CO = 0.028004129375259,
  NO = 0.030003710178288,
  NO2 = 0.046000354695573,
  O3 = 0.047989939272703,
  SO2 = 0.064046570680133
)




mass_to_parts <- function(values, parameter, temperature = 20.0, pressure = 1013.15, ...) {
  parameter <- as.character(parameter) # no factor shenigans
  if (tibble::has_name(mol_masses, parameter)) {
    values * molar_gas_constant * (temperature + 273.15) / pressure / 100 / mol_masses[[parameter]]
  } else {
    values
  }
}

parts_to_mass <- function(values, parameter, temperature = 20.0, pressure = 1013.15, ...) {
  parameter <- as.character(parameter) # no factor shenigans
  if (tibble::has_name(mol_masses, parameter)) {
    values / molar_gas_constant / (temperature + 273.15) * pressure * 100 * mol_masses[[parameter]]
  } else {
    values
  }
}



lookup_unit_conversion <- list(
  "mg/m3" = list(new_unit = "ppm", fun = mass_to_parts),
  "µg/m3" = "pbb",
  "ppm" = "mg/m3"
)

convert <- function(data, ...) {
  parameter <- dplyr::first(data$parameter)
  unit <- dplyr::first(data$unit)


}


conversion_table <- tibble::tribble(
  ~parameter, ~from, ~to, ~FUN,
  "CO", "ppm", "mg/m3", mass_to_parts,
  "NO", "ppb", "µg/m3", mass_to_parts,
  "O3", "ppb", "µg/m3", mass_to_parts,
  "NO2", "ppb", "µg/m3", mass_to_parts,
  "SO2", "ppb", "µg/m3", mass_to_parts,
  "CO", "mg/m3", "ppm", parts_to_mass,
  "NO", "µg/m3", "ppb", parts_to_mass,
  "O3", "µg/m3", "ppb", parts_to_mass,
  "NO2", "µg/m3", "ppb", parts_to_mass,
  "SO2", "µg/m3", "ppb", parts_to_mass
)





