context("apply meta data universal")

fn <- system.file("extdata", "smn.txt", package = "rOstluft.data", mustWork = TRUE)
meta_fn <- system.file("extdata", "meta_smn.rds", package = "rOstluft.data", mustWork = TRUE)

data <- read_smn(fn, na.rm = FALSE)
meta <- readRDS(meta_fn)

test_that("strict", {
  testthat::expect_error(
    meta_apply(data, meta, "parameter", NULL, "parameter_original", "parameter")
  )

  testthat::expect_error(
    meta_apply(data, NULL, "asdf", "parameter", "parameter_original", "parameter")
  )

  # should fail: missing mapping for rre150z0
  testthat::expect_error(
    meta_apply(data, meta, "parameter", "parameter", "parameter_original", "parameter")
  )

  res <- dplyr::filter(data, .data$parameter != "rre150z0")

  # go the whole way, rename parameter, site and add parameter > unit mapping
  res <- meta_apply(res, meta, "parameter", "unit", "parameter_original", "unit")
  res <- meta_apply(res, meta, "parameter", "parameter", "parameter_original", "parameter")
  res <- meta_apply(res, meta, "site", "site", "site_short", "site")

  res <- dplyr::distinct(res, .data$site, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  cmp <- dplyr::filter(meta, .data$parameter %in% res$parameter, .data$site %in% res$site) %>%
    dplyr::select("site", "parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  testthat::expect_equal(nrow(res), 9)
  testthat::expect_equal(res, cmp)
})


test_that("drop", {
  testthat::expect_warning(
    res <- meta_apply(data, meta, "parameter", "unit", "parameter_original", "unit", mode = "drop")
  )

  # after drop we don't expect another warning
  res <- meta_apply(res, meta, "parameter", "parameter", "parameter_original", "parameter", mode = "drop")

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
  testthat::expect_warning(
    res <- meta_apply(data, meta, "parameter", "unit", "parameter_original", "unit", mode = "keep")
  )

  testthat::expect_warning(
    res <- meta_apply(res, meta, "parameter", "parameter", "parameter_original", "parameter", mode = "keep")
  )

  # site mapping is in meta
  res <- meta_apply(res, meta, "site", "site", "site_short", "site",  mode = "keep")

  res <- dplyr::distinct(res, .data$site, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  cmp <- dplyr::filter(meta, .data$parameter %in% res$parameter, .data$site %in% res$site) %>%
    dplyr::select("site", "parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(tibble::tibble(site = "Chur", parameter = "rre150z0", unit = as.character(c(NA)))) %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  testthat::expect_equal(nrow(res), 10)
  testthat::expect_equal(res, cmp)
})


test_that("replace", {
  testthat::expect_error(
    res <- meta_apply(data, meta, "parameter", "unit", "parameter_original", "unit", mode = "replace")
  )

  testthat::expect_message(
    res <- meta_apply(data, meta, "parameter", "unit", "parameter_original", "unit", mode = "replace",
                      replacements = list(rre150z0 = "unit1", dkl010z0 = "unit2"))
  )

  testthat::expect_message(
    res <- meta_apply(res, meta, "parameter", "parameter", "parameter_original", "parameter", mode = "replace",
                      replacements = list(rre150z0 = "par1", dkl010z0 = "par2"))
  )

  # site mapping is in meta
  res <- meta_apply(res, meta, "site", "site", "site_short", "site",  mode = "replace")

  res <- dplyr::distinct(res, .data$site, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  cmp <- dplyr::filter(meta,
      .data$parameter %in% res$parameter, .data$site %in% res$site, .data$parameter_original != "dkl010z0"
    ) %>%
    dplyr::select("site", "parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::filter() %>%
    dplyr::bind_rows(tibble::tibble(site = "Chur", parameter = c("par1", "par2"), unit = c("unit1", "unit2"))) %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  testthat::expect_equal(nrow(res), 10)
  testthat::expect_equal(res, cmp)

  testthat::expect_equal(nrow(res), 10)
  testthat::expect_equal(res, cmp)
})

test_that("works with character inputs", {
  res <- dplyr::mutate_if(data, is.factor, as.character)

  testthat::expect_error(
    meta_apply(res, meta, "parameter", NULL, "parameter_original", "parameter")
  )

  testthat::expect_error(
    meta_apply(data, NULL, "asdf", "parameter", "parameter_original", "parameter")
  )

  # should fail: missing mapping for rre150z0
  testthat::expect_error(
    meta_apply(data, meta, "parameter", "parameter", "parameter_original", "parameter")
  )

  res <- dplyr::filter(data, .data$parameter != "rre150z0")

  # go the whole way, rename parameter, site and add parameter > unit mapping
  res <- meta_apply(res, meta, "parameter", "unit", "parameter_original", "unit")
  res <- meta_apply(res, meta, "parameter", "parameter", "parameter_original", "parameter")
  res <- meta_apply(res, meta, "site", "site", "site_short", "site")

  res <- dplyr::distinct(res, .data$site, .data$parameter, .data$unit) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  cmp <- dplyr::filter(meta, .data$parameter %in% res$parameter, .data$site %in% res$site) %>%
    dplyr::select("site", "parameter", "unit") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$site, .data$parameter, .data$unit)

  testthat::expect_equal(nrow(res), 9)
  testthat::expect_equal(res, cmp)
})


test_that("catch duplicates in meta_key and replacements", {
  meta_dupl <- dplyr::mutate_if(meta, is.factor, as.character) %>%
    dplyr::distinct(.data$parameter_original, .data$unit) %>%
    dplyr::bind_rows(tibble::tibble(parameter_original = "dkl010z0", unit = "dupl"))

  # catch duplicates in meta
  testthat::expect_error(
    res <- meta_apply(data, meta_dupl, "parameter", "unit", "parameter_original", "unit", mode = "replace",
                      replacements = list(rre150z0 = "unit1", rre150z0 = "dupl"))
  )

  # catch duplicates in replacements
  testthat::expect_error(
    res <- meta_apply(data, meta, "parameter", "unit", "parameter_original", "unit", mode = "replace",
                      replacements = list(rre150z0 = "unit1", rre150z0 = "dupl"))
  )
})

