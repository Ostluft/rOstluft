context("openair <> rolf")

h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv",
                  package = "rOstluft.data", mustWork = TRUE)

d1 <- system.file("extdata", "Zch_Stampfenbachstrasse_d1_2013_Jan.csv",
                  package = "rOstluft.data", mustWork = TRUE)

airmo_h1 <- read_airmo_csv(h1)
airmo_d1 <- read_airmo_csv(d1)
airmo_mixed <- bind_rows_with_factor_columns(airmo_h1, airmo_d1)

test_that("rolf > openair single", {
  oa_co <- rolf_to_openair_single(airmo_mixed, "CO", "mg/m3", "d1")
  oa_p  <- rolf_to_openair_single(dplyr::filter(airmo_d1, parameter == "p"), keep_interval = TRUE)

  testthat::expect_equal(dim(oa_co), c(31, 4))
  testthat::expect_equivalent(sort(rlang::names2(oa_co)), sort(c("date", "unit", "CO", "site")))
  testthat::expect_equal(dim(oa_p), c(31, 5))

  testthat::expect_error(
    rolf_to_openair_single(airmo_mixed, "CO", "mg/m3")
  )
  testthat::expect_error(
    rolf_to_openair_single(airmo_mixed, "CO", interval = "d1")
  )

  testthat::expect_error(
    rolf_to_openair_single(airmo_mixed, unit = "mg/m3", interval = "d1")
  )
})


test_that("rolf <> openair", {
  oa_d1 <- rolf_to_openair(airmo_d1)
  oa_h1 <- rolf_to_openair(airmo_mixed, interval = "h1")
  oa_list <- rolf_to_openair(airmo_mixed, interval = "h1", as_list = TRUE, keep_ppb = TRUE, keep_interval = TRUE)

  testthat::expect_equivalent(dim(oa_d1), c(31, 64))
  testthat::expect_equivalent(dim(oa_h1), c(744, 15))
  testthat::expect_true(all(c("ws", "wd") %in% rlang::names2(oa_h1))) # check auto renaming of wind params
  testthat::expect_equal(length(oa_list), 19)

  testthat::expect_error(
    rolf_to_openair(airmo_mixed)
  )

  testthat::expect_error(
    rolf_to_openair(airmo_d1, keep_ppb = TRUE)
  )

  units_h1 <- attr(oa_h1, "units")
  units_d1 <- attr(oa_d1, "units")

  res_h1 <- openair_to_rolf(oa_h1, interval = "h1", units = units_h1)
  testthat::expect_true(all(c("WVv", "WD") %in% res_h1$parameter)) # check auto renaming of wind params

  res_d1 <- openair_to_rolf(oa_d1, interval = "d1", units = units_d1)
  cmp_d1 <- dplyr::filter(airmo_d1, !(.data$unit == "ppb" | .data$unit == "ppm")) %>%
    dplyr::arrange(.data$parameter, .data$starttime)

  testthat::expect_equal(
    cmp_d1$value,
    dplyr::arrange(res_d1, .data$parameter, .data$starttime)$value
  )

  res_list <- bind_rows_with_factor_columns(!!!purrr::map(oa_list, openair_to_rolf))
  testthat::expect_equal(  # devtools::check fails this test (somewhere shifted by one), devtools::test not?
    dplyr::arrange(airmo_h1, .data$parameter, .data$unit, .data$site, .data$starttime)$value,
    dplyr::arrange(res_list, .data$parameter, .data$unit, .data$site, .data$starttime)$value
  )

  testthat::expect_true(all(c("WVv", "WD") %in% res_list$parameter)) # check auto renaming of wind params

  testthat::expect_error(
    openair_to_rolf(oa_d1, interval = "d1", units = units_h1)
  )

})
