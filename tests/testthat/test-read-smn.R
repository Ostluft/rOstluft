context("read swiss meteo network")

single <- system.file("extdata", "smn.txt", package = "rOstluft.data", mustWork = TRUE)
multi <- system.file("extdata", "smn_multi.txt", package = "rOstluft.data", mustWork = TRUE)
number_of_chunks_in_multi <- 24


setup({
  tempdir(check = TRUE)
})

teardown({
  tmp_files <- fs::dir_ls(tempdir(), regexp = fs::path_file(multi))
  purrr::map(tmp_files, fs::file_delete)
})


test_that("correct content", {
  res <- read_smn(single, na.rm = FALSE)
  testthat::expect_equal(dim(res), c(60, 6))
  testthat::expect_equal(sum(is.na(res$value)), 6)

  # probably should check columns and classes but the check should be implemented in the format class
})


test_that("split file", {
  tmp_dir <- tempdir()

  testthat::expect_message(
    split_smn(multi, tmp_dir),
    regexp = sprintf("Finished file with %2d chunks", number_of_chunks_in_multi)
  )

  chunk_files <- fs::dir_ls(tempdir(), regexp = fs::path_file(multi))
  testthat::expect_equal(length(chunk_files), number_of_chunks_in_multi)
})


test_that("read alot of single files", {
  chunk_files <- fs::dir_ls(tempdir(), regexp = fs::path_file(multi))
  res <- purrr::map(chunk_files, read_smn)

  # we simple test that every file contained some data
  test <- purrr::map(res, ~ tibble::is_tibble(.) && nrow(.) > 0)
  testthat::expect_equal(length(res), number_of_chunks_in_multi)
  testthat::expect_true(all(as.logical(test)), label = "every file contained some data")
})


test_that("read one file with alot of single exports", {
  res <- read_smn_multiple(multi, as_list = TRUE)

  # we simple test that every chunk contained some data
  test <- purrr::map(res, ~ tibble::is_tibble(.) && nrow(.) > 0)
  testthat::expect_equal(length(res), number_of_chunks_in_multi)
  testthat::expect_true(all(as.logical(test)), label = "every chunk contained some data")
})







