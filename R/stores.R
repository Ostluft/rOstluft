#' Predefined stores
#'
#' * `store_aqmet()` - Ostluft S3 storage containing airquality and meteo data
#' * `store_hysplit()` - Ostluft S3 storage containing NOAA Hysplit trajectories
#'
#' @param name of the store (to influence caching location)
#'
#' @return store
#'
#' @rdname stores
#' @export
store_aqmet <- function(name = "aqmet") {
  storage_s3_rds(name, format = format_rolf(), bucket = "rostluft", prefix = "aqmet")
}

#' @rdname stores
#' @export
store_hysplit <- function(name = "hysplit") {
  storage_s3_rds(name, format_hysplit(),  bucket = "rostluft", prefix = "hysplit")
}
