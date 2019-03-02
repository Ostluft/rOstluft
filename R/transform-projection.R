#' Map projection and datum transformation
#'
#' @param df data frame containing the coordinate information
#' @param coord columns of the coordinates. Default `c("lon", "lat")`
#' @param initCRS source coordinate reference system as [sp::CRS]. Default `sp::CRS("+init=epsg:4326")`
#' @param outCRS target coordinate reference system as [sp::CRS]. Default `sp::CRS("+init=epsg:2056")`
#'
#' @return data frame containing the transformed coordinatess
#' @export
#'
#' @seealso
#' * [sp::spTransform()]
#' * [sp::proj4string()]
#' * [sp::CRS]
#' * [PROJ4](https://proj4.org/)
transform_projection <- function(df, coord = c("lon", "lat"), initCRS = sp::CRS("+init=epsg:4326"),
                                 outCRS = sp::CRS("+init=epsg:2056")) { #' CRS("+init=epsg:4326") = WGS84, CRS("+init=epsg:2056") = LV95
  sp::coordinates(df) <- coord
  sp::proj4string(df) <- initCRS
  df <- as.data.frame(sp::spTransform(df, outCRS))
  return(df)
}

