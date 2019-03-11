context("conversion")

min30 <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2013_Jan.csv",
                     package = "rOstluft.data", mustWork = TRUE)

airmo_min30 <- read_airmo_csv(min30)
# filter special case NOx and PM10
airmo_min30 <- dplyr::filter(airmo_min30, .data$parameter != "NOx", .data$parameter != "PM10")
airmo_min30_parts <- dplyr::filter(airmo_min30, .data$unit == "ppb" | .data$unit == "ppm")
airmo_min30_mass <- dplyr::filter(airmo_min30, .data$unit == "µg/m3" | .data$unit == "mg/m3")

conversions_mass_to_parts <- tibble::tribble(
  ~parameter, ~from, ~to,
  "CO", "mg/m3", "ppm",
  "NO", "µg/m3", "ppb",
  "O3", "µg/m3", "ppb",
  "NO2", "µg/m3", "ppb",
  "SO2", "µg/m3", "ppb"
)

conversions_parts_to_mass <- tibble::tribble(
  ~parameter, ~from, ~to,
  "CO", "ppm", "mg/m3",
  "NO", "ppb", "µg/m3",
  "O3", "ppb", "µg/m3",
  "NO2", "ppb", "µg/m3",
  "SO2", "ppb", "µg/m3"
)

# get_molmass <- function(parameter, mass, parts) {
#   mass <- dplyr::filter(mass, parameter == !!parameter)$value
#   parts <- dplyr::filter(parts, parameter == !!parameter)$value
#   statistic_mean(calculate_molmass(mass, parts))
# }


expect_equal_values <- function(parameter, unit, input, output) {
  testthat::expect_equal(
    dplyr::filter(input, .data$parameter == !!parameter, .data$unit == !! unit)$value,
    dplyr::filter(output, .data$parameter == !!parameter, .data$unit == !! unit)$value,
    tolerance = 1e-5,
    info = sprintf("testing parameter %s and unit %s", parameter, unit)
  )
}


test_that("parts to mass math", {
  res_mass <- convert_multiple_units(airmo_min30_parts, conversions_parts_to_mass, method = "return")

  for (i in 1:nrow(conversions_parts_to_mass)) {
    row <- conversions_parts_to_mass[i, ]
    expect_equal_values(row$parameter, row$to, res_mass, airmo_min30_mass)
  }
})

test_that("mass to parts math", {
  res_parts <- convert_multiple_units(airmo_min30_mass, conversions_mass_to_parts, method = "return")

  for (i in 1:nrow(conversions_mass_to_parts)) {
    row <- conversions_mass_to_parts[i, ]
    expect_equal_values(row$parameter, row$to, res_parts, airmo_min30_parts)
  }
})

test_that("functionality single conversion", {
  testthat::expect_error(
    convert_unit(airmo_min30_parts, "xyz", "from_unit", "to_unit", method = "return")
  )


  row <- conversions_parts_to_mass[2, ]

  n <- nrow(airmo_min30_parts)
  n_par <- nrow(dplyr::filter(airmo_min30_parts, .data$parameter == row$parameter, .data$unit == row$from))

  testthat::expect_equal(
    nrow(convert_unit(airmo_min30_parts, row$parameter, row$from, row$to, method = "replace")),
    n
  )

  testthat::expect_equal(
    nrow(convert_unit(airmo_min30_parts, row$parameter, row$from, row$to, method = "return")),
    n_par
  )

  testthat::expect_equal(
    nrow(convert_unit(airmo_min30_parts, row$parameter, row$from, row$to, method = "append")),
    n_par + n
  )
})

test_that("functionality multi conversion", {
  airmo_min30_without_mass <- dplyr::filter(airmo_min30, !(.data$unit == "µg/m3" | .data$unit == "mg/m3"))

  n <- nrow(airmo_min30_without_mass)
  n_par <- nrow(dplyr::filter(airmo_min30, .data$unit == "µg/m3" | .data$unit == "mg/m3"))

  testthat::expect_equal(
    nrow(convert_multiple_units(airmo_min30_without_mass, conversions_parts_to_mass, method = "replace")),
    n
  )

  testthat::expect_equal(
    nrow(convert_multiple_units(airmo_min30_without_mass, conversions_parts_to_mass, method = "return")),
    n_par
  )

  testthat::expect_equal(
    nrow(convert_multiple_units(airmo_min30_without_mass, conversions_parts_to_mass, method = "append")),
    n_par + n
  )
})

test_that("passing of additional argumens", {
  row <- conversions_parts_to_mass[2, ]

  temp <- ((20 + 273.15) * 1.1) - 273.15  # nolint 10% wärmere temperatur -> 10% weniger masse
  pres <- 0.9 * 1013  # 10% weniger durck -> 10% weniger masse
  factor <- 1.1 / 0.9

  # single conversion
  res <- convert_unit(airmo_min30_parts, row$parameter, row$from, row$to, method = "return")
  res_add <- convert_unit(airmo_min30_parts, row$parameter, row$from, row$to, method = "return",
                          temperature = temp, pressure = pres)

  testthat::expect_equal(
    res$value,
    res_add$value * factor
  )

  # multi conversion, test replace and return different implementations
  res <- convert_multiple_units(airmo_min30_parts, conversions_parts_to_mass, method = "return")
  res_return <- convert_multiple_units(airmo_min30_parts, conversions_parts_to_mass, method = "return",
                                       temperature = temp, pressure = pres)
  res_replace <- convert_multiple_units(airmo_min30_parts, conversions_parts_to_mass, method = "replace",
                                        temperature = temp, pressure = pres)


  testthat::expect_equal(
    res$value,
    res_return$value * factor
  )

  testthat::expect_equal(
    res$value,
    res_replace$value * factor
  )

})
