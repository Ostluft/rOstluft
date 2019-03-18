context("statstable")


test_that("a lot of stats", {

  statstable <- tibble::tribble(
    ~parameter, ~statistic, ~from, ~to,
    "default_statistic", "mean", "input", "h1,d1,m1,y1",
    "default_statistic", "max, min, n", "input", "d1,m1,y1",
    "RainDur", "sum", "input", "h1,d1,m1,y1",
    "RainDur", "n", "input", "d1,m1,y1",
    "WD", "wind.direction", "input", "h1,d1,m1,y1",
    "WVs", "wind.speed_scalar", "input", "h1,d1,m1,y1",
    "WVv", "wind.speed_vector", "input", "h1,d1,m1,y1",
    "WD", "drop", "input", "h1",
    "WVs", "drop", "input", "h1",
    "WVv", "drop", "input", "h1",
    "WD", "n", "input", "d1,m1,y1",
    "WVs", "max, min, n", "input", "d1,m1,y1",
    "WVv", "max, min, n", "input", "d1,m1,y1",
    "default_statistic", "perc95", "input", "y1",
    "O3", "mean, min, max, n, perc02, perc98", "input", "y1",
    "O3", "max, min, n, n>120, n>160, n>180, n>200, n>240", "h1", "d1,m1,y1",
    "T", "max", "h1", "d1,m1,y1",
    "NO2", "max, n>200", "h1", "d1,m1,y1",
    "O3", "mean", "h1", "h8gl",
    "CO", "mean", "h1", "h8gl",
    "CO", "max", "h8gl", "y1",
    "O3", "max, n>120, n>160, n>180, n>200, n>240", "h8gl", "y1",
    "_inputs_", "min, max, n", "d1", "y1",
    "RainDur", "drop", "d1", "y1",
    "WD", "drop", "d1", "y1",
    "StrGlo", "max, n", "d1", "y1",
    "Hr", "drop", "d1", "y1",
    "p", "drop", "d1", "y1",
    "WVv", "drop", "d1", "y1",
    "CO", "min, max, n, n>8", "d1", "y1",
    "SO2", "min, max, n, n>100", "d1", "y1",
    "NO2", "min, max, n, n>80", "d1", "y1",
    "PM10", "min, max, n, n>50", "d1", "y1",
    "PM2.5", "min, max, n, n>25", "d1", "y1",
    "O3", "min, max, n, n>120, n>160, n>200, n>180, n>240, n>65", "d1", "y1",
    "O3_max_h1", "n>120, n>160, n>180, n>200, n>240", "d1", "m1,y1"
  )

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
  stats_airmo <- bind_rows_with_factor_columns(h1, d1, m1, y1)
  stats_airmo <- dplyr::group_by(stats_airmo, parameter, interval, unit) %>% dplyr::slice(1)
  stats_airmo <- dplyr::ungroup(stats_airmo)
  stats_airmo <- dplyr::mutate_if(stats_airmo, is.factor, as.character)
  stats_airmo$parameter <- purrr::map_chr(stats_airmo$parameter, relabel_perc)
  stats_airmo$parameter <- dplyr::recode(stats_airmo$parameter, "O3_max_h8gl_ugz" = "O3_max_h8gl")


  ### actuel code to test ###
  input <- read_airmo_csv(fn_in)
  input <- calculate_mass_concentrations(input)
  stats <- calculate_statstable(input, statstable)
  stats <- bind_rows_with_factor_columns(!!!stats)

  # just take the first value of every statistic calculated (we did the same with the airmo stats)
  stats <- dplyr::group_by(stats, parameter, interval, unit) %>% dplyr::slice(1)
  stats <- dplyr::ungroup(stats)
  stats <- dplyr::mutate_if(stats, is.factor, as.character)

  # rename days with hours > xxx

  mapping <- list(
    "O3_max_h1_nb_d1>120" = "O3_nb_d1_mit_h1>120",
    "O3_max_h1_nb_d1>160" = "O3_nb_d1_mit_h1>160",
    "O3_max_h1_nb_d1>180" = "O3_nb_d1_mit_h1>180",
    "O3_max_h1_nb_d1>200" = "O3_nb_d1_mit_h1>200",
    "O3_max_h1_nb_d1>240" = "O3_nb_d1_mit_h1>240"
  )

  stats <- dplyr::mutate(stats, parameter = dplyr::recode(.data$parameter, !!!mapping))


  # match all the stats
  res <- dplyr::inner_join(stats, stats_airmo, suffix = c("", ".AIRMO"),
                           by=c("starttime", "site", "parameter", "interval", "unit"))


  # readr::write_tsv(res, "statstable_matched.tsv")

  testthat::expect_equal(
    res$value,
    res$value.AIRMO,
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
  # readr::write_tsv(no_match, "st_airmo_statistics_with_no_match_in_rostluft.tsv")
  # readr::write_tsv(no_match2, "st_rostluft_statistics_with_no_match_in_airmo.tsv")
})











