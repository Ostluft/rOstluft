transform.projection <- function(df, coord = c("lon", "lat"), initCRS = sp::CRS("+init=epsg:4326"),
                                 outCRS = sp::CRS("+init=epsg:2056")) { #' CRS("+init=epsg:4326") = WGS84, CRS("+init=epsg:2056") = LV95
  sp::coordinates(df) <- coord
  sp::proj4string(df) <- initCRS
  df <- as.data.frame(sp::spTransform(df, outCRS))
  return(df)
}
