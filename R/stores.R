#' @title Predefined stores
#'
#' @description
#' * `store_aqmet()` - Ostluft S3 storage containing airquality and meteo data
#' * `store_aqmet_public()` - Public Ostluft S3 storage containing airquality and meteo data.
#' * `store_hysplit()` - Ostluft S3 storage containing NOAA Hysplit trajectories
#'
#' @section store_aqmet_public:
#' This store contains data from following sources:
#'
#' ## [Ostluft](https://www.ostluft.ch/)
#'
#' Ostluft is a inter cantonal measurement network of eastern switzerland and the Principality of Liechtenstein.
#'
#' ## [ETHZ IAC](http://www.iac.ethz.ch/the-institute/weather-stations.html)
#'
#' Institute for Atmosheric and Climate Science (IAC), ETH Zurich
#'
#' This data can be used freely for scientific analysis.
#' The user agrees to cite IAC ETH Zurich as source of the data and to consult IAC ETH Zurich before data is used for publication.
#'
#' Please note that we do not take any guarantee for the correctness and completeness of the available data.
#'
#' ## [National Air Pollution Monitoring Network (NABEL)](https://www.bafu.admin.ch/bafu/en/home/topics/air/state/data/national-air-pollution-monitoring-network--nabel-.html)
#'
#' The National Air Pollution Monitoring Network (NABEL) measures air pollution at 16 locations in Switzerland.
#' The stations are distributed throughout the country and monitor pollution at typical locations (e.g.
#' city-centre streets, residential areas, rural stations). The monitoring network has commenced operations in stages
#' since 1979 and is operated by the Federal office for the environment and Empa.
#'
#' ## [Water police Zuerich](https://data.stadt-zuerich.ch/dataset/sid_wapo_wetterstationen)
#'
#' The water police Zuerich operates two meteo station. The locations are Tiefenbrunnen and Mythenquai.
#'
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
  storage_s3_rds(name, format = format_hysplit(),  bucket = "rostluft", prefix = "hysplit")
}


#' @rdname stores
#' @export
store_aqmet_public <- function(name = "aqmet_public") {
  storage_s3_rds(
    name,
    format = format_rolf(),
    bucket = "rostluftpublic",
    prefix = "aqmet_public",
    region = "eu-central-1"
  )
}
