#' Transform between Coordinate Reference Systems
#'
#' @section WSG84 (EPSG 4326) vs Web Pseudo Mercator (EPSG 3857):
#' See [Tiles à la Google Maps](https://www.maptiler.com/google-maps-coordinates-tile-bounds-projection/) for a
#' detailed explanation. In Short:
#'
#' EPSG:4326 uses a coordinate system on the surface of a sphere or ellipsoid of reference.
#'
#' EPSG:3857 uses a coordinate system PROJECTED from the surface of the sphere or ellipsoid to a flat surface.
#'
#' Usually Packages and Webservices expects coordinates for Elements in EPSG:4326, one Exception is
#' [leaflet::addRasterImage()]
#'
#' @section Known Issues:
#' R spatial is migrating to PROJ6+ and gdal3+. At the moment the sp::spTransform isn't converted to
#' use the WKT comments, and rgdal raises some Warnings. The transformation are done correctly.
#' * [R spatial follows GDAL and PROJ development](https://www.r-spatial.org/r/2020/03/17/wkt.html)
#' * [Migration to PROJ6/GDAL3](http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html#impacts-of-gdal-barnraising-on-sp-workflows-1)
#' * [rOstluft Issue](https://github.com/Ostluft/rOstluft/issues/7)
#'
#' @param data input data
#' @param coord mappping of columns for Conversion
#' @param in_crs source coordinate reference system as [sp::CRS]
#' @param out_crs target coordinate reference system as [sp::CRS]
#' @param append append data in new Columns
#'
#' @return tibble with transformed coordinates
#' @export
#'
#' @seealso
#' * [sp::spTransform()]
#' * [sp::proj4string()]
#' * [sp::CRS]
#' * [PROJ4](https://proj4.org)
#' * [epsg.io: Coordinate Systems Worldwide](https://epsg.io)
#' * [Working with projections in Leaflet](https://rstudio.github.io/leaflet/projections.html)
#' * [Tiles à la Google Maps](https://www.maptiler.com/google-maps-coordinates-tile-bounds-projection/)
#'
#' @examples
#' fn <- system.file("extdata", "meta_smn.rds", package = "rOstluft.data")
#' data <- readRDS(fn)
#' data <- dplyr::distinct(data, site, x, y)
#' transform_crs(
#'   data = data,
#'   coord = c(lon = "x", lat = "y"),
#'   in_crs = sp::CRS(SRS_string = "EPSG:2056"),
#'   out_crs = sp::CRS(SRS_string = "EPSG:4326")
#' )
#'
#' transform_crs(
#'   data = data,
#'   coord = c(lon = "x", lat = "y"),
#'   in_crs = sp::CRS(SRS_string = "EPSG:2056"),
#'   out_crs = sp::CRS(SRS_string = "EPSG:4326"),
#'   append = FALSE
#' )
#'
#' transform_LV95_to_WSG84(data)
#'
transform_crs <- function(data, coord, in_crs, out_crs, append = TRUE) {
  out_cols <- rlang::names2(coord)
  if (any(out_cols == "") || length(out_cols) != 2) {
    stop("coord must be a named character vector with 2 items")
  }

  if (isTRUE(append)) {
    data <- dplyr::bind_cols(data, dplyr::select(data, !!!coord))
  } else {
    data <- dplyr::rename(data, !!!coord)
  }
  sp::coordinates(data) <- out_cols
  sp::proj4string(data) <- in_crs
  data <- sp::spTransform(data, out_crs)
  tibble::as_tibble(data)
}

#' @rdname transform_crs
#' @export
transform_LV95_to_WSG84 <- function(data, coord = c(lon = "x", lat = "y"), append = TRUE) {
  transform_crs(data, coord, sp::CRS(SRS_string = "EPSG:2056"), sp::CRS(SRS_string = "EPSG:4326"), append)
}

#' @rdname transform_crs
#' @export
transform_WSG84_to_LV95 <- function(data, coord = c(x = "lon", y = "lat"), append = TRUE) {
  transform_crs(data, coord, sp::CRS(SRS_string = "EPSG:4326"), sp::CRS(SRS_string = "EPSG:2056"), append)
}

#' @rdname transform_crs
#' @export
transform_LV03_to_WSG84 <- function(data, coord = c(lon = "x", lat = "y"), append = TRUE) {
  transform_crs(data, coord, sp::CRS(SRS_string = "EPSG:21781"), sp::CRS(SRS_string = "EPSG:4326"), append)
}

#' @rdname transform_crs
#' @export
transform_WSG84_to_LV03 <- function(data, coord = c(x = "lon", y = "lat"), append = TRUE) {
  transform_crs(data, coord, sp::CRS(SRS_string = "EPSG:4326"), sp::CRS(SRS_string = "EPSG:21781"), append)
}
