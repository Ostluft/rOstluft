context("read swiss meteo network")

single <- system.file("extdata", "smn.txt", package = "rOstluft.data", mustWork = TRUE)
unit <- system.file("extdata", "smn_unit.txt", package = "rOstluft.data", mustWork = TRUE)
multi <- system.file("extdata", "smn_multi.txt", package = "rOstluft.data", mustWork = TRUE)
web <- system.file("extdata", "smn_VQHA80.txt", package = "rOstluft.data", mustWork = TRUE)
new <- system.file("extdata", "smn_neues_format.txt", package = "rOstluft.data", mustWork = TRUE)



number_of_chunks_in_multi <- 28

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
  # 2018-01-01T00:00:00+00:00 10min MW mit Endtime == 2018-01-01T00:50:00+01:00 Startzeit
  testthat::expect_equal(res$starttime[1], lubridate::ymd_hms("2018-01-01T00:50:00", tz = "Etc/GMT-1"))

  # manual shifting of start time
  res <- read_smn(single, time_shift = lubridate::period(-20, units = "minutes"), na.rm = FALSE)
  testthat::expect_equal(res$starttime[1], lubridate::ymd_hms("2018-01-01T00:40:00", tz = "Etc/GMT-1"))

  # manual interval, makes manual shifting necessary
  testthat::expect_error(
    res <- read_smn(single, interval = "10 minutes", na.rm = FALSE)
  )

  res <- read_smn(single, interval = "10 minutes", time_shift = lubridate::period(+10, units = "minutes"))
  testthat::expect_equal(res$starttime[1], lubridate::ymd_hms("2018-01-01T01:10:00", tz = "Etc/GMT-1"))
  testthat::expect_equal(as.character(res$interval[1]), "10 minutes" )

  # read file with unit information
  res <- read_smn(unit, na.rm = FALSE)
  testthat::expect_equal(dim(res), c(60, 6))
  testthat::expect_equal(sum(is.na(res$value)), 6)

  # read the web export from https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/info/VQHA80_de.txt
  res <- read_smn(web, interval = "min10", time_shift = lubridate::period(-10, "min"), na.rm = FALSE)
  testthat::expect_equal(dim(res), c(3180, 6))

  res <- read_smn(new, na.rm = FALSE)
  testthat::expect_equal(dim(res), c(10*16, 6))
  testthat::expect_equal(sum(is.na(res$value)), 16)


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

  # one chunk contains no data
  res <- purrr::map(chunk_files, purrr::possibly(read_smn, NULL))

  # we simple test that every file contained some data
  test <- purrr::keep(res, ~ tibble::is_tibble(.) && nrow(.) > 0)
  testthat::expect_equal(length(res), number_of_chunks_in_multi)
})


test_that("read one file with alot of single exports", {
  res <- read_smn_multiple(multi, as_list = TRUE)

  # we simple test that every chunk contained some data
  test <- purrr::map(res, ~ tibble::is_tibble(.) && nrow(.) > 0)
  testthat::expect_equal(length(res), number_of_chunks_in_multi)
  testthat::expect_true(all(as.logical(test)), label = "every chunk contained some data")
})
