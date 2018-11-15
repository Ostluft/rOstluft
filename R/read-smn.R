# !!! work in progress

# ! noch anpassen mit readr Funktionen, richtigem Zeichensatz encoding und tz
# todo: Funktion zum MeteoSchweiz-Daten formatieren, optimieren & doku


# aufbau sollte sein:
# funktion die nur die konventierung von input nach format rolf übernimmt
# automatischer download aktueller daten von ostluft als wrapper
# fast copy & paste



#' Reads a Swiss Meteo Network data file
#'
#' Reads the data from a Swiss Meteo Network export and returns it as a data frame in rOstluft long format
#'
#' @param file Either a path to a file, a connection, or literal data. see [readr::read_delim()]
#' @param encoding file encoding. Default "latin1"
#' @param tz time zone of date fields. Be carefull Etc/GMT + == -. Default "Etc/GMT-1"
#' @param time_shift a lubridate period to add to the time. Default NULL
#'
#' @return data frame in rOstluft long format
#'
#' @seealso [lubridate::period()] - Create or parse period objects
#' @seealso [base::timezones] - Information about time zones in R
#'
#' @export
read_smn_10min <- function(file, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL) {
  invisible(NULL)
}


read_smn_cap <- function(file, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL) {
  invisible(NULL)
}


#' Utility function to generate url and optional caching
#'
#' @param file
#' @param cache if NULL no caching, if TRUE the data is cached in the user data dir, anything
#'   else is used to create a path relative to current dir
#' @param userpw
#'
#' @return
#' @export
get_smn_con <- function(file, cache = NULL, userpw = NULL) {
  # problem for cache:
  # we need always a base url to create a mapping from url to cache
  # create a internal r6 class or function set for this
  # then create hard coded wrappers for specific data locations

  # can't lookup url from home
  base_url <- "ftp://ftp.ostluft.ch/data_MeteoCH/Archiv/ftp_original/"

  # no caching -> just return url readr automatically download http(s)/ftp(s) urls
  if (is.null(cache)) {
    return(RCurl::getURL(fs::path(base_url, file), userpw = userpw))
  }

  if (isTRUE(cache)) {
    cache_path <- rappdirs::user_data_dir("cache", "rOstluft", "smn")
  } else {
    cache_path <- fs::path_abs(cache)
  }

  cache_path <- fs::path(cache_path, file)

  if (!fs::file_exists(cache_path)) {
    # download file with httr or rcurl to disk, message/warning/error by fail, argument as flag which case to use?
    # resp <- httr::HEAD(chunk_url)
    # if (httr::status_code(resp) == 200) {
    #   httr::GET(chunk_url, httr::write_disk(chunk_path, overwrite = TRUE))
    # }
  }
  return(cache_path)
}



read_MeteoCH_10min <- function(file, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL) {
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
