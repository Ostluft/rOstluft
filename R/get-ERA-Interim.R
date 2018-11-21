# !!! work in progress


require(reticulate)
require(ncdf4)
require(ncdf4.helpers)
require(PCICt)



allyears <- 1979:(year(today()) - 1)# - months(3)))
yearfiles <- unique(as.numeric(substring(list.files("netcdf", pattern = "_interim.nc"), 1, 4)))
if (length(yearfiles) > 0) {
  years <- allyears[!(allyears %in% yearfiles)]
} else {
  years <- 1979:(year(today()) - 1)# - months(3)))
}



if (length(years) > 0) {

  query <- lapply(years, function(x) paste0(

    "#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer

server = ECMWFDataServer()

server.retrieve({
  'class': 'ei',
  'dataset': 'interim',
  'date': '", x,"-01-01/to/", x,"-12-31',
  'expver': '1',
  'grid': '0.75/0.75',
  'levtype': 'sfc',
  'param': '39.128/134.128/136.128/137.128/139.128/141.128/164.128/165.128/166.128/167.128/168.128/173.128/206.128/235.128',
  'step': '0', # 0 = reanalysis data (1 - 3 = forecast timesteps)
  'stream': 'oper',
  'time': '00:00:00/06:00:00/12:00:00/18:00:00',
  'type': 'an',
  'area': '48/5/45.5/11', # => Schweiz; area:  N/W/S/E
  'format': 'netcdf',
  'target': 'netcdf/", x,"_00061218_analysis_interim.nc'
})
server.retrieve({
  'class': 'ei',
  'dataset': 'interim',
  'date': '", x,"-01-01/to/", x,"-12-31',
  'expver': '1',
  'grid': '0.75/0.75',
  'levtype': 'sfc',
  'param': '39.128/57.128/58.128/59.128/134.128/136.128/137.128/139.128/141.128/144.128/146.128/147.128/159.128/164.128/165.128/166.128/167.128/168.128/176.128/177.128/182.128/189.128/206.128/210.128/211.128/228.128/231.128/235.128',
  'step': '12',
  'stream': 'oper',
  'time': '00:00:00/12:00:00',
  'type': 'fc',
  'area': '48/5/45.5/11', # => Schweiz; area:  N/W/S/E
  'format': 'netcdf',
  'target': 'netcdf/", x,"_0012_forecast12_interim.nc'
})"

  ))



  lapply(query, function(x) {
    writeLines(x, "query.py")
    source_python("query.py")
  })



  ### see e.g. https://cran.r-project.org/web/packages/futureheatwaves/vignettes/starting_from_netcdf.html

  files <- list.files("netcdf", pattern = "forecast12_interim.nc")
  df <- lapply(files, function(x) {


    netcdf <- nc_open(paste0("netcdf/", x))


    # netcdf
    # netcdf$dim$time$units
    # netcdf$dim$time$calendar
    # netcdf$dim$latitude$units
    # netcdf$dim$longitude$units


    lon <- ncvar_get(netcdf, varid = "longitude")
    lat <- ncvar_get(netcdf, varid = "latitude")
    time <- with_tz(as.POSIXct(nc.get.time.series(netcdf, v = "t2m", time.dim.name = "time")), tzone = "UTC")
    lon_index <- which.min(abs(lon - 8.53035)) # gidcell closest to Zurich
    lat_index <- which.min(abs(lat - 47.3776)) # ..


    ### ... see print(netcdf)
    y <- data.frame(
      time = time,
      lat = lat[lat_index],
      lon = lon[lon_index],
      t2m = as.numeric(nc.get.var.subset.by.axes(netcdf, "t2m", axis.indices = list(X = lon_index, Y = lat_index)) - 273.15), # air temperature @ 2 m (deg C)
      swvl1 = as.numeric(nc.get.var.subset.by.axes(netcdf, "swvl1", axis.indices = list(X = lon_index, Y = lat_index))), # volumetric soil water content layer 1
      sp = as.numeric(nc.get.var.subset.by.axes(netcdf, "sp", axis.indices = list(X = lon_index, Y = lat_index)) / 100), # surface air pressure (hPa)
      u10 = as.numeric(nc.get.var.subset.by.axes(netcdf, "u10", axis.indices = list(X = lon_index, Y = lat_index))),
      v10 = as.numeric(nc.get.var.subset.by.axes(netcdf, "v10", axis.indices = list(X = lon_index, Y = lat_index))),
      skt = as.numeric(nc.get.var.subset.by.axes(netcdf, "skt", axis.indices = list(X = lon_index, Y = lat_index)) - 273.15), # Skin temperature (°C)
      ishf = as.numeric(nc.get.var.subset.by.axes(netcdf, "ishf", axis.indices = list(X = lon_index, Y = lat_index))), # Instantaneous surface sensible heat flux
      tp = as.numeric(nc.get.var.subset.by.axes(netcdf, "tp", axis.indices = list(X = lon_index, Y = lat_index))), # Total precipitation
      tco3 = as.numeric(nc.get.var.subset.by.axes(netcdf, "tco3", axis.indices = list(X = lon_index, Y = lat_index))), # Total column ozone
      sund = as.numeric(nc.get.var.subset.by.axes(netcdf, "sund", axis.indices = list(X = lon_index, Y = lat_index))), # Sunshine duration
      e = as.numeric(nc.get.var.subset.by.axes(netcdf, "e", axis.indices = list(X = lon_index, Y = lat_index))), # Evaporation
      d2m = as.numeric(nc.get.var.subset.by.axes(netcdf, "d2m", axis.indices = list(X = lon_index, Y = lat_index)) - 273.15), # 2 metre dewpoint temperature (°C)
      tcc = as.numeric(nc.get.var.subset.by.axes(netcdf, "tcc", axis.indices = list(X = lon_index, Y = lat_index))), # Total cloud cover
      blh = as.numeric(nc.get.var.subset.by.axes(netcdf, "blh", axis.indices = list(X = lon_index, Y = lat_index))), # Boundary layer height
      slhf = as.numeric(nc.get.var.subset.by.axes(netcdf, "slhf", axis.indices = list(X = lon_index, Y = lat_index))), # Surface latent heat flux
      sshf = as.numeric(nc.get.var.subset.by.axes(netcdf, "sshf", axis.indices = list(X = lon_index, Y = lat_index))), # Surface sensible heat flux
      sd = as.numeric(nc.get.var.subset.by.axes(netcdf, "sd", axis.indices = list(X = lon_index, Y = lat_index))), # Snow depth
      stl1 = as.numeric(nc.get.var.subset.by.axes(netcdf, "stl1", axis.indices = list(X = lon_index, Y = lat_index)) - 273.15), # Soil temperature level 1 (°C)
      tcwv = as.numeric(nc.get.var.subset.by.axes(netcdf, "tcwv", axis.indices = list(X = lon_index, Y = lat_index))), # Total column water vapour
      cape = as.numeric(nc.get.var.subset.by.axes(netcdf, "cape", axis.indices = list(X = lon_index, Y = lat_index))), # Convective available potential energy
      par = as.numeric(nc.get.var.subset.by.axes(netcdf, "par", axis.indices = list(X = lon_index, Y = lat_index))), # Photosynthetically active radiation at the surface
      uvb = as.numeric(nc.get.var.subset.by.axes(netcdf, "uvb", axis.indices = list(X = lon_index, Y = lat_index))) # Downward UV radiation at the surface
    ) %>%
      mutate(
        ws = sqrt(u10^2 + v10^2),
        wd = (270 - atan2(u10, v10) * 180/pi) %% 360
      )


    nc_close(netcdf)


    return(y)
  })


  units <- c(
    "t2m" = "°C",
    "swvl1" = "m3/m3",
    "sp" = "hPa",
    "u10" = "m/s",
    "v10" = "m/s",
    "skt" = "°C",
    "ishf" = "W/m2",
    "tp" = "m",
    "tco3" = "kg/m2",
    "sund" = "s",
    "e" = "m H2O",
    "d2m" = "°C",
    "tcc" = "-",
    "blh" = "m",
    "slhf" = "J/m2",
    "sshf" = "J/m2",
    "sd" = "m H2O",
    "stl1" = "°C",
    "tcwv" = "kg/m2",
    "cape" = "J/kg",
    "par" = "J/m2",
    "uvb" = "J/m2",
    "ws" = "m/s",
    "wd" = "°"
  )


  df <- bind_rows(df) %>%
    gather(par, val, -time, -lat, -lon) %>%
    mutate(
      unit = plyr::revalue(par, units),
      site = "Zürich - ECMWF"
    ) %>%
    dplyr::select(-lat, -lon)

}
