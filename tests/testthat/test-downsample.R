context("downsample")

test_that("simple", {
  skip("need to implement it proper")
  store <- driver_rds_local("test2")
  co <- store$get("Zch_Stampfenbachstrasse", "min30", 2011, "CO")
  downsample_series(co, "d1", data.thresh = 0.8)

})
