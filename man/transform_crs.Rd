% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform-projection.R
\name{transform_crs}
\alias{transform_crs}
\alias{transform_LV95_to_WSG84}
\alias{transform_WSG84_to_LV95}
\alias{transform_LV03_to_WSG84}
\alias{transform_WSG84_to_LV03}
\title{Transform between Coordinate Reference Systems}
\usage{
transform_crs(data, coord, in_crs, out_crs, append = TRUE)

transform_LV95_to_WSG84(data, coord = c(lon = "x", lat = "y"), append = TRUE)

transform_WSG84_to_LV95(data, coord = c(x = "lon", y = "lat"), append = TRUE)

transform_LV03_to_WSG84(data, coord = c(lon = "x", lat = "y"), append = TRUE)

transform_WSG84_to_LV03(data, coord = c(x = "lon", y = "lat"), append = TRUE)
}
\arguments{
\item{data}{input data}

\item{coord}{mappping of columns for Conversion}

\item{in_crs}{source coordinate reference system as \link[sp:CRS-class]{sp::CRS}}

\item{out_crs}{target coordinate reference system as \link[sp:CRS-class]{sp::CRS}}

\item{append}{append data in new Columns}
}
\value{
tibble with transformed coordinates
}
\description{
Transform between Coordinate Reference Systems
}
\section{WSG84 (EPSG 4326) vs Web Pseudo Mercator (EPSG 3857)}{

See \href{https://www.maptiler.com/google-maps-coordinates-tile-bounds-projection/}{Tiles à la Google Maps} for a
detailed explanation. In Short:

EPSG:4326 uses a coordinate system on the surface of a sphere or ellipsoid of reference.

EPSG:3857 uses a coordinate system PROJECTED from the surface of the sphere or ellipsoid to a flat surface.

Usually Packages and Webservices expects coordinates for Elements in EPSG:4326, one Exception is
\code{\link[leaflet:addRasterImage]{leaflet::addRasterImage()}}
}

\section{Known Issues}{

R spatial is migrating to PROJ6+ and gdal3+. At the moment the sp::spTransform isn't converted to
use the WKT comments, and rgdal raises some Warnings. The transformation are done correctly.
\itemize{
\item \href{https://www.r-spatial.org/r/2020/03/17/wkt.html}{R spatial follows GDAL and PROJ development}
\item \href{http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html#impacts-of-gdal-barnraising-on-sp-workflows-1}{Migration to PROJ6/GDAL3}
\item \href{https://github.com/Ostluft/rOstluft/issues/7}{rOstluft Issue}
}
}

\examples{
fn <- system.file("extdata", "meta_smn.rds", package = "rOstluft.data")
data <- readRDS(fn)
data <- dplyr::distinct(data, site, x, y)
transform_crs(
  data = data,
  coord = c(lon = "x", lat = "y"),
  in_crs = sf::st_crs("EPSG:2056"),
  out_crs = sf::st_crs("EPSG:4326")
)

transform_crs(
  data = data,
  coord = c(lon = "x", lat = "y"),
  in_crs = sf::st_crs("EPSG:2056"),
  out_crs = sf::st_crs("EPSG:4326"),
  append = FALSE
)

transform_LV95_to_WSG84(data)

}
\seealso{
\itemize{
\item \code{\link[sp:spTransform]{sp::spTransform()}}
\item \code{\link[sp:is.projected]{sp::proj4string()}}
\item \link[sp:CRS-class]{sp::CRS}
\item \href{https://proj4.org}{PROJ4}
\item \href{https://epsg.io}{epsg.io: Coordinate Systems Worldwide}
\item \href{https://rstudio.github.io/leaflet/projections.html}{Working with projections in Leaflet}
\item \href{https://www.maptiler.com/google-maps-coordinates-tile-bounds-projection/}{Tiles à la Google Maps}
}
}
