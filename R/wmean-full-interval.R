

#' calculates weighted arithmetic means for full / even time intervals
#'
#' Sometimes records (e.g. measurements from passive samplers or miniDOAS) provide values representative for time intervals such as
#' starting from odd start times to odd end times (e.g. 09:58 to 10:08 or 20.02. to 06.03.) with respect to full / even time intervals
#' for mean intervals (e.g. 09:00 to 10:00 or 01.02. to 28.02.). To make such time series intercomparable
#' and provide a standardized way of dealing with aggregated data, wmean_full_interval() provides a method to average
#' a data.frame containing start- and end-time information to full / even time intervals based on stats::weighted.mean().
#'
#' @param df data.frame for averaging; df has to be in long format and contain a start- and end-time column of class POSIXct (arbitrarily named)
#' @param startinterval string; name of start-time column
#' @param endinterval string; name of end-time column
#' @param value string; name of column containing values to be averaged
#' @param groups string, optional => can be NULL; name of column containing values for grouping (using group_by()) when calculating weighted means (e.g. for different measurement parameters)
#' @param res object of class "lubridate::Period" specifying the intervall for averaging (e.g. lubridate::hours(1))
#'
#' @return data.frame with columns for interval start-time ("start"), interval end-time ("end"), "groups" (optional), "mean", "n" (sum of weighted intervals within the averaged time interval)
#'
#' @keywords statistics
wmean_full_interval <- function(df, startinterval, endinterval, value, groups = NULL, res = lubridate::hours(1)) {

  # to-do: incomplete first and last intervals have to be removed; deal with only NA containing intervals

  stopifnot(is.period(res))
  stopifnot(is.POSIXct(df[,startinterval]) & is.POSIXct(df[,endinterval]))
  if (!is.data.frame(df)) {data.frame(df)}
  if (res@minute + res@hour > 0) {
    base <- xts::align.time(df[,startinterval], n = as.numeric(res)) - as.numeric(res)
  }
  if (res@day > 0) {
    base <- df[,startinterval]
    second(base) <- 0
    minute(base) <- 0
    hour(base) <- 0
  }
  if (res@month > 0) {
    base <- df[,startinterval]
    second(base) <- 0
    minute(base) <- 0
    hour(base) <- 0
    day(base) <- 1
  }
  df2 <- data.frame(
    start = df[,startinterval],
    end = df[,endinterval],
    start_mean = base,
    end_mean = base + res,
    w = 1,
    dplyr::select(df, -one_of(c(startinterval, endinterval)))
  )
  index <- which(df2$start > df2$start_mean & df2$end > df2$end_mean)

  if (length(index) > 0) {
    frac2 <- as.numeric(df2$end - df2$end_mean, units = "secs") / as.numeric(df2$end - df2$start, units = "secs")
    frac1 <- 1 - frac2
    df2[index, "w"] <- frac1[index]
    df3 <- df2[index,]
    df3$w <- frac2[index]
    df3$start2 <- df3$end_mean
    d <- bind_rows(df2, df3)
  } else {
    d <- df2
  }

  dfw <- d %>%
    dplyr::rename(val = !!(value)) %>%
    dplyr::select(-start, -end, -end_mean) %>%
    mutate(start = start_mean) %>%
    filter(!is.na(start)) %>%
    dplyr::select(-start_mean, start, w, everything()) %>%
    group_by_at(vars(c("start", groups))) %>%
    # summarise_at(2:(1+ncol(df)),
    summarise(
      # mean = funs(weighted.mean(., w, na.rm = TRUE)),
      mean = weighted.mean(val, w, na.rm = TRUE),
      n = sum(w, na.rm = TRUE)
    ) %>%
    ungroup()

  dfw <- dfw %>%
    # right_join(seq.POSIXt(min(dfw$start, na.rm = TRUE), max(dfw$start, na.rm = TRUE), res))) %>%
    expand_(c("start", groups)) %>%
    right_join(dfw, by = c("start", groups)) %>%
    mutate(end = start + res) %>%
    dplyr::select(start, end, everything())

  return(dfw)

}

