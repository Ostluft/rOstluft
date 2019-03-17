
test_that("a lot of stats", {

  statstable <- tibble::tribble(
    ~parameter, ~statistic, ~from, ~to,
    "default_statistic", "mean", "input", "h1,d1,m1,y1",
    "RainDur", "sum", "input", "h1,d1,m1,y1",
    "WD", "wind.direction", "input", "h1,d1,m1,y1",
    "WVs", "wind.speed_scalar", "input", "h1,d1,m1,y1",
    "WVv", "wind.speed_vector", "input", "h1,d1,m1,y1",
    "WVs", "max", "h1", "d1,m1,y1",
    "NO", "max", "input", "h1,d1,m1,y1",
    "NO", "max", "h1", "d1,m1,y1",
    "O3", "mean,max", "input", "h1,d1,m1,y1",
    "O3", "max,n>120", "h1", "d1,m1,y1",
    "O3", "mean", "h1", "h8gl",
    "CO", "mean", "h1", "h8gl",
    "O3_max_h1", "n>120", "d1", "d1,m1,y1",
    "CO", "max", "h8gl", "y1",
    "O3", "max", "h8gl", "y1"
  )

  fn_input <- system.file("extdata", "Zch_Stampfenbachstrasse_min30_2017.csv", package = "rOstluft.data", mustWork = TRUE)
  input <- read_airmo_csv(fn_input)
  input <- calculate_mass_concentrations(input)
  res <- calculate_statstable(input, statstable)

  testthat::expect_type(res, "list")


})











