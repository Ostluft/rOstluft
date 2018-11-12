# !!! work in progress

# ! noch anpassen mit readr Funktionen, richtigem Zeichensatz encoding und tz
# todo: Funktion zum MeteoSchweiz-Daten formatieren, optimieren & doku



read_MeteoCH_10min <- function(x, url, userpwd) {
  con <- getURL(paste0(url, x), userpw = userpwd)
  # write.table(con, paste0("data_MeteoCH/Archiv/ftp_original/", x,".txt"), row.names = FALSE, col.names = FALSE)
  skip <- which(read_lines(con) == "stn time               ta1tows0  uretowhs  prestas0  fk1towz0  fkltowz1  dkltowz1  sre000z0  gre000z0")
  df1 <- fread(con, sep = " ", skip = "stn", header = TRUE, blank.lines.skip = TRUE, fill = TRUE, encoding = "UTF-8", na.strings = "-", data.table = FALSE)[2:(skip-2),] %>%
    mutate_if(is.numeric, as.character) %>%
    gather(par, val, -1, -2)
  df2 <- fread(con, sep = " ", skip = skip-1, header = TRUE, blank.lines.skip = TRUE, fill = TRUE, encoding = "UTF-8", na.strings = "-", data.table = FALSE)[-1,] %>%
    mutate_if(is.numeric, as.character) %>%
    gather(par, val, -1, -2)
  df <- bind_rows(df1, df2) %>%
    mutate(
      time = fast_strptime(time, format = "%Y%m%d%H%M", tz = "UTC", lt = FALSE),
      interval = "10min"
    )
  return(df)
}



read_MeteoCH_CAP <- function(x, url, userpwd) {
  con <- getURL(paste0(url, x), userpw = userpwd)
  df <- fread(con, sep = " ", skip = 4, header = TRUE, blank.lines.skip = TRUE, fill = TRUE, encoding = "UTF-8", na.strings = "-", data.table = FALSE) %>%
    mutate(
      time = fast_strptime(time, format = "%d.%m.%Y", tz = "UTC", lt = FALSE),
      interval = "1d"
    ) %>%
    gather(par, val, -time, -interval)
  return(df)
}


### Funktion ETHZ IAC Daten einlesen
read_ethz_10min <- function(x, url, userpwd, site) {
  cat(paste0(format(x), "\n"))
  try({
    con <- getURL(paste0(url,"IAC-Met",site,"_", x,".dat"), userpw = userpwd)
    df <- fread(con, sep = " ", skip = 40, header = TRUE, blank.lines.skip = TRUE, fill = TRUE, encoding = "UTF-8", na.strings = "99999.0", data.table = FALSE) %>%
      mutate(date = x)
  })
  return(df)
}

### Funktion ETHZ IAC Daten formatieren
format_ethz_10min <- function(x, site) {
  try({
    y <- x %>%
      slice(-1) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(time = as.POSIXct(date + minutes(time), tz = "UTC")) %>%
      # dplyr::rename(
      #   rh = RH,
      #   Tair = T_air,
      #   ws = wspd_scl,
      #   ws_max = wspd_pk,
      #   wd = wdir,
      #   p = p_air
      # ) %>%
      # dplyr::select(time, Tair, rh, p, wd, ws, ws_max) %>%
      gather(par, val, -time) %>%
      mutate(
        time = with_tz(time, "Etc/GMT-1"),
        site = site,
        interval = "10min"
      )
  })
  return(y)
}


