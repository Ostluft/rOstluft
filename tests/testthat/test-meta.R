context("apply meta data")

fn <- system.file("extdata", "smn.txt", package = "rOstluft.data", mustWork = TRUE)
meta_fn <- system.file("extdata", "meta_smn.rds", package = "rOstluft.data", mustWork = TRUE)

data <- read_smn(fn, na.rm = FALSE)
meta <- readRDS(meta_fn)


test_that("strict", {

  # should fail: missing mapping for rre150z0
  testthat::expect_error(rolf_apply_meta(data, meta, "parameter_original", "parameter", "unit"))

  res <- dplyr::filter(data, .data$parameter != "rre150z0")
  res <- rolf_apply_meta(res, meta, "parameter_original", "parameter", "unit")
  res <- dplyr::distinct(res, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$parameter)

  cmp <- dplyr::filter(meta, .data$parameter %in% res$parameter) %>%
    dplyr::select("parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$parameter)

  testthat::expect_equal(nrow(res), 9)
  testthat::expect_equal(res, cmp)
})


test_that("drop", {
  testthat::expect_warning(
    res <- rolf_apply_meta(data, meta, "parameter_original", "parameter", "unit", mode = "drop")
  )

  res <- dplyr::distinct(res, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$parameter)

  cmp <- dplyr::filter(meta, .data$parameter %in% res$parameter) %>%
    dplyr::select("parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$parameter)

  testthat::expect_equal(nrow(res), 9)
  testthat::expect_equal(res, cmp)
})


test_that("keep", {
  testthat::expect_message(
    res <- rolf_apply_meta(data, meta, "parameter_original", "parameter", "unit", mode = "keep")
  )

  res <- dplyr::distinct(res, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$parameter)

  cmp <- dplyr::filter(meta, .data$parameter %in% res$parameter) %>%
    dplyr::select("parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(tibble::tibble(parameter = "rre150z0", unit = as.character(c(NA)))) %>%
    dplyr::arrange(.data$parameter)

  testthat::expect_equal(nrow(res), 10)
  testthat::expect_equal(res, cmp)
})


test_that("replace", {
  testthat::expect_error(
    res <- rolf_apply_meta(data, meta, "parameter_original", "parameter", "unit", mode = "replace")
  )


  testthat::expect_message(
    res <- rolf_apply_meta(data, meta, "parameter_original", "parameter", "unit", mode = "replace",
                           replacement_parameter = list("rre150z0" = "new_par"),
                           replacement_unit = list("rre150z0" = "new_unit"))
  )

  res <- dplyr::distinct(res, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$parameter)

  cmp <- dplyr::filter(meta, .data$parameter %in% res$parameter) %>%
    dplyr::select("parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(tibble::tibble(parameter = "new_par", unit = "new_unit")) %>%
    dplyr::arrange(.data$parameter)

  testthat::expect_equal(nrow(res), 10)
  testthat::expect_equal(res, cmp)
})


