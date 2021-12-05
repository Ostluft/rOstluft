test_that("test transform_LV95_to_WSG84", {
  # berechnet mit navref https://www.swisstopo.admin.ch/de/karten-daten-online/calculation-services/navref.html
  navref <- tibble::tribble(
    ~x,          ~y,          ~lon,        ~lat,
    2710517.000, 1259824.000, 8.904915239, 47.479893903,
    2661904.000, 1203410.000, 8.252246153, 46.978873852
  )

  input <- dplyr::select(navref, x, y)

  testthat::expect_warning(
    result <- transform_LV95_to_WSG84(input)
  )

  testthat::expect_equal(navref, result, ignore_attr = TRUE)
})


test_that("test transform_WSG84_to_LV95", {
  # berechnet mit navref https://www.swisstopo.admin.ch/de/karten-daten-online/calculation-services/navref.html
  navref <- tibble::tribble(
    ~x,          ~y,          ~lon,        ~lat,
    2710517.001, 1259824.001, 8.904915239, 47.479893903,
    2661904.001, 1203410.001, 8.252246153, 46.978873852
  )

  input <- dplyr::select(navref, lon, lat)
  testthat::expect_warning(
    result <- transform_WSG84_to_LV95(input)
  )
  result <- dplyr::select(result, x, y, lon, lat)

  testthat::expect_equal(navref, result, ignore_attr = TRUE)
})
