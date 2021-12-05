
write_missing_stats <- FALSE



test_that("O3 summer stats", {

  fn_in <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2017.csv", package = "rOstluft.data", mustWork = TRUE)
  input <- read_airmo_csv(fn_in)

  stats <- calculate_O3_summer(input)

  aot40_airmo <- 7680.63501234568  # in airmo aot40 has interval m6,4-9 and starttime of first april
  o3h7_airmo <- 74.26676082        # copied from airmo @ 21.05.2019

  aot40 <- dplyr::filter(stats, parameter == "AOT40")$value
  o3h7 <- dplyr::filter(stats, parameter == "O3_h709001600")$value

  testthat::expect_equal(
    aot40,
    aot40_airmo,
    tolerance = 1e-6
  )

  testthat::expect_equal(
    o3h7,
    o3h7_airmo,
    tolerance = 0.2
  )
})



test_that("a lot of stats", {

  statstable <- statstable_default()

  fn_in <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2017.csv", package = "rOstluft.data", mustWork = TRUE)
  fn_h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2017.csv", package = "rOstluft.data", mustWork = TRUE)
  fn_d1 <- system.file("extdata", "Zch_Stampfenbachstrasse_d1_2017.csv", package = "rOstluft.data", mustWork = TRUE)
  fn_m1 <- system.file("extdata", "Zch_Stampfenbachstrasse_m1_2017.csv", package = "rOstluft.data", mustWork = TRUE)
  fn_y1 <- system.file("extdata", "Zch_Stampfenbachstrasse_y1_2017.csv", package = "rOstluft.data", mustWork = TRUE)

  h1 <- read_airmo_csv(fn_h1, na.rm = FALSE)
  d1 <- read_airmo_csv(fn_d1, na.rm = FALSE)
  m1 <- read_airmo_csv(fn_m1, na.rm = FALSE)
  y1 <- read_airmo_csv(fn_y1, na.rm = FALSE)

  relabel_perc <- function (level) {
    if (stringr::str_ends(level, "%")) {
      level <- stringr::str_c(level, "_min30")
    }

    level
  }

  # prepate comparsion data
  stats_airmo <- dplyr::bind_rows(h1, d1, m1, y1)
  stats_airmo <- dplyr::group_by(stats_airmo, parameter, interval, unit) %>% dplyr::slice(1)
  stats_airmo <- dplyr::ungroup(stats_airmo)
  stats_airmo <- dplyr::mutate_if(stats_airmo, is.factor, as.character)
  stats_airmo$parameter <- purrr::map_chr(stats_airmo$parameter, relabel_perc)
  stats_airmo$parameter <- dplyr::recode(
    stats_airmo$parameter,
    "O3_max_h8gl_ugz" = "O3_max_h8gl",
    "O3_nb_98%_m1>100" = "O3_98%_min30_nb_m1>100",
    "O3_max_98%_m1" = "O3_98%_min30_max_m1"
  )

  ### actuel code to test ###
  input <- read_airmo_csv(fn_in)
  stats <- calculate_stats(input)

  # just take the first value of every statistic calculated (we did the same with the airmo stats)
  stats <- dplyr::group_by(stats, parameter, interval, unit) %>% dplyr::slice(1)
  stats <- dplyr::ungroup(stats)
  stats <- dplyr::mutate_if(stats, is.factor, as.character)

  # match all the stats
  res <- dplyr::inner_join(stats, stats_airmo, suffix = c("", ".AIRMO"),
                           by = c("starttime", "site", "parameter", "interval", "unit"))

  if (isTRUE(write_missing_stats)) {
    readr::write_tsv(res, "statstable_matched.tsv")
  }


  testthat::expect_equal(
    res$value,
    res$value.AIRMO,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )

  if (isTRUE(write_missing_stats)) {
    #find the lonely ones
    no_match <- dplyr::anti_join(stats_airmo, stats, suffix = c("", ".AIRMO"),
                                 by = c("starttime", "site", "parameter", "interval", "unit"))

    no_match2 <- dplyr::anti_join(stats, stats_airmo, suffix = c("", ".AIRMO"),
                                  by = c("starttime", "site", "parameter", "interval", "unit"))


    readr::write_tsv(no_match, "st_airmo_statistics_with_no_match_in_rostluft.tsv")
    readr::write_tsv(no_match2, "st_rostluft_statistics_with_no_match_in_airmo.tsv")
  }
})
