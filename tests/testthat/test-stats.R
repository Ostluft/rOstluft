context("stats")


# we just try to match as many stats from airmo to the one we calculate
test_that("a lot of stats", {
  fn_input <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2017.csv",
                          package = "rOstluft.data", mustWork = TRUE)

  fn_h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2017.csv",
                       package = "rOstluft.data", mustWork = TRUE)

  fn_d1 <- system.file("extdata", "Zch_Stampfenbachstrasse_d1_2017.csv",
                       package = "rOstluft.data", mustWork = TRUE)

  fn_m1 <- system.file("extdata", "Zch_Stampfenbachstrasse_m1_2017.csv",
                       package = "rOstluft.data", mustWork = TRUE)

  fn_y1 <- system.file("extdata", "Zch_Stampfenbachstrasse_y1_2017.csv",
                       package = "rOstluft.data", mustWork = TRUE)

  h1 <- read_airmo_csv(fn_h1, na.rm = FALSE)
  d1 <- read_airmo_csv(fn_d1, na.rm = FALSE)
  m1 <- read_airmo_csv(fn_m1, na.rm = FALSE)
  y1 <- read_airmo_csv(fn_y1, na.rm = FALSE)
  input <- read_airmo_csv(fn_input)

  relabel_perc <- function (level) {
    if (stringr::str_ends(level, "%")) {
      level <- stringr::str_c(level, "_min30")
    }
    level
  }

  stats_airmo <- bind_rows_with_factor_columns(h1, d1, m1, y1)
  stats_airmo <- dplyr::mutate_if(stats_airmo, is.factor, as.character)
  stats_airmo$parameter <- purrr::map_chr(stats_airmo$parameter, relabel_perc)
  stats_airmo$parameter <- dplyr::recode(stats_airmo$parameter,
                                         O3_max_h8gl_ugz = "O3_max_h8gl"
  )

  input <- calculate_mass_concentrations(input)
  stats <- calculate_stats(input)
  stats_lrv <- calculate_LRV(input)
  stats_o3 <- calculate_O3(input)
  stats_co_h8gl <- calculate_CO_h8gl(input)
  stats_o3_h8gl <- calculate_O3_h8gl(input)

  stats <- bind_rows_with_factor_columns(!!!stats, !!!stats_lrv, !!!stats_o3,
                                         !!!stats_o3_h8gl, !!!stats_co_h8gl)

  # just take the first value of every statistic calculated (we did the same with the airmo stats)
  stats <- dplyr::group_by(stats, parameter, interval, unit) %>% dplyr::slice(1)

  stats <- dplyr::ungroup(stats)
  stats <- dplyr::mutate_if(stats, is.factor, as.character)

  # match all the stats
  res <- dplyr::inner_join(stats, stats_airmo, suffix = c("", ".AIRMO"),
                           by=c("starttime", "site", "parameter", "interval", "unit"))

  ### readr::write_tsv(res, "matched.tsv")

  O3_h709001600 <- dplyr::filter(res, parameter == "O3_h709001600")
  res <- dplyr::filter(res, parameter != "O3_h709001600")

  testthat::expect_equal(
    res$value,
    res$value.AIRMO,
    tolerance = 1e-6
  )

  testthat::expect_equal(
    O3_h709001600$value,
    O3_h709001600$value.AIRMO,
    tolerance = 1e-6
  )

  # find the lonely ones
  # no_match <- dplyr::anti_join(stats_airmo, stats, suffix = c("", ".AIRMO"),
  #                              by=c("starttime", "site", "parameter", "interval", "unit"))
  #
  # no_match2 <- dplyr::anti_join(stats, stats_airmo, suffix = c("", ".AIRMO"),
  #                               by=c("starttime", "site", "parameter", "interval", "unit"))
  #
  #
  # readr::write_tsv(no_match, "airmo_statistics_with_no_match_in_rostluft.tsv")
  # readr::write_tsv(no_match2, "rostluft_statistics_with_no_match_in_airmo.tsv")
})
