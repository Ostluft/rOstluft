#' calculates weighted arithmetic means to match full time intervals from overlapping intervals
#'
#' Sometimes records (e.g. measurements from passive samplers or miniDOAS) provide values representative for time intervals such as
#' starting from odd start times to odd end times (e.g. 09:58 to 10:08 or 20 Feb to 06 March) with respect to full time intervals
#' for mean intervals (e.g. 09:00 to 10:00 or 01 Feb to 28 Feb). To make such time series intercomparable
#' and provide a standardized way of dealing with aggregated data, wmean_full_interval() provides a method to average
#' a data.frame containing start- and end-time information to full time intervals based on stats::weighted.mean().
#'
#' @param df data.frame for averaging; df has to be in long format and contain a start- and end-time column of class POSIXct (arbitrarily named)
#' @param startinterval string; name of start-time column
#' @param endinterval string; name of end-time column
#' @param value string; name of column containing values to be averaged
#' @param groups string, optional => can be NULL; name of column containing values for grouping (using dplyr::group_by()) when calculating weighted means (e.g. for different measurement parameters)
#' @param res object of class "lubridate::Period" specifying the intervall for averaging (e.g. lubridate::hours(1))
#'
#' @return data.frame with columns for interval start-time ("start"), interval end-time ("end"), "groups" (optional), "mean", "n" (sum of weighted intervals within the averaged time interval)
#'
#' @keywords statistics
wmean_full_interval <- function(df, startinterval = "starttime", endinterval = "endtime", value = "value", groups = NULL, res = lubridate::hours(1)) {

  # to-do: incomplete first and last intervals have to be removed; deal with only NA containing intervals
  # stopifnot(is.period(res))
  # stopifnot(is.POSIXct(pull(df[,startinterval])) & is.POSIXct(pull(df[,endinterval])))

  df2 <-
    df %>%
    dplyr::select(!!!startinterval, !!!endinterval) %>%
    dplyr::mutate(
      start_mean = lubridate::floor_date(!!rlang::sym(startinterval), unit = res),
      end_mean = start_mean + res,
      w = 1
    ) %>%
    dplyr::rename(
      start = startinterval,
      end = endinterval
    )
  df2 <- dplyr::bind_cols(df2, dplyr::select(df, -dplyr::one_of(c(startinterval, endinterval))))
  index <- which(df2$start > df2$start_mean & df2$end > df2$end_mean)

  if (length(index) > 0) {
    frac2 <- as.numeric(df2$end - df2$end_mean, units = "secs") / as.numeric(df2$end - df2$start, units = "secs")
    frac1 <- 1 - frac2
    df2[index, "w"] <- frac1[index]
    df3 <- df2[index,]
    df3$w <- frac2[index]
    df3$start2 <- df3$end_mean
    df2 <- dplyr::bind_rows(df2, df3)
  }

  dfw <- dplyr::rename(df2, val = !!(value))
  dfw <- dplyr::select(dfw, -start, -end, -end_mean)
  dfw <- dplyr::mutate(dfw, start = start_mean)
  dfw <- dplyr::filter(dfw, !is.na(start))
  dfw <- dplyr::select(dfw, -start_mean, start, w, dplyr::everything())
  dfw <- dplyr::group_by_at(dfw, dplyr::vars(c("start", groups)))
  # dplyr::summarise_at(2:(1+ncol(df)),
  dfw <- dplyr::summarise(dfw,
                          # mean = funs(weighted.mean(., w, na.rm = TRUE)),
                          mean = weighted.mean(val, w, na.rm = TRUE),
                          n = sum(w, na.rm = TRUE)
  )
  dfw <- dplyr::ungroup(dfw)

  dfw <-
    dfw %>%
    # dplyr::right_join(seq.POSIXt(min(dfw$start, na.rm = TRUE), max(dfw$start, na.rm = TRUE), res))) %>%
    tidyr::expand_(c("start", groups)) %>%
    dplyr::right_join(dfw, by = c("start", groups)) %>%
    dplyr::mutate(end = start + res) %>%
    dplyr::select(start, end, dplyr::everything())

  return(dfw)

}



fill_ps <- function(data, interval = "h1") {
  interval_converted <- convert_interval(interval)

  data <- dplyr::mutate(data,
    starttime = lubridate::floor_date(.data$starttime, interval_converted),
    endtime  = lubridate::floor_date(.data$endtime, interval_converted),
  )

  data <- dplyr::group_nest(data, .data$site, .data$parameter, .data$unit, .key = "serie")
  data <- dplyr::mutate(data, serie = purrr::map(.data$serie, fill_ps_serie, interval_converted))
  data <- tidyr::unnest(data, .data$serie)
  data <- dplyr::mutate(data, interval = factor(interval))

  dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
}


fill_ps_serie <- function(serie, interval) {
  purrr::pmap_dfr(serie, fill_ps_messung, interval)
}



fill_ps_messung <- function(starttime, endtime, value, interval) {
  tibble::tibble(
    starttime = seq(starttime, endtime, interval),
    value = value
  ) %>% dplyr::slice(-dplyr::n())
}

