#' Merge two data frames in rOstluft long format into one
#'
#' Combines the two data frames. Defaulting to data frame `y`.
#'
#' @param x data.frame in rOstluft long format
#' @param y data.frame in rOstluft long format
#'
#' @return merged data frame
#' @export
merge_rOstluft_longformat <- function(x, y) {
  # for some reason, factors can be coerced to character vectors
  suppressWarnings(
    dfn <- dplyr::full_join(y, x, by = c("startzeit", "station", "parameter", "intervall", "einheit"),
                     suffix = c("", ".old"))
  )
  dfn[["wert"]][is.na(dfn["wert"])] <- dfn[["wert.old"]][is.na(dfn["wert"])]
  dfn <- dplyr::select(dfn, -.data$wert.old)
  dfn <- dplyr::arrange(dfn, .data$startzeit)
  dplyr::mutate_if(dfn, is.character, as.factor)
}



#' bind rows of data frames with factors
#'
#' [dplyr::bind_rows()] coerces factors to characters. This function use the [forcats::fct_c()]
#' to combine the factor columns.
#'
#' _Important_: It is mandatory that all data frames have the same structure
#'
#' @param ... data frames to combine
#'
#' @return combined data frame
#'
#' @export
bind_rows_with_factor_columns <- function(...) {
  purrr::pmap_df(list(...), function(...) {
    cols_to_bind <- list(...)
    if (all(purrr::map_lgl(cols_to_bind, is.factor))) {
      forcats::fct_c(!!!cols_to_bind)
    } else {
      purrr::invoke(c, .x = cols_to_bind)
    }
  })
}



#' Merge two data frame with store content statistic into one
#'
#' Combines the two data frames. Defaulting to data frame `y`.
#'
#' @param x data frame
#' @param y data frame
#'
#' @return combined data frame
#'
#' @keywords internal
merge_content <- function(x, y) {
  # for some reason, factors can be coerced to character vectors
  suppressWarnings(
    dfn <- dplyr::full_join(y, x, by = c("station", "parameter", "intervall", "einheit", "jahr"),
                            suffix = c("", ".old"))
  )
  dfn[["n"]][is.na(dfn["n"])] <- dfn[["n.old"]][is.na(dfn["n"])]
  dfn <- dplyr::select(dfn, -.data$n.old)
  dplyr::mutate_if(dfn, is.character, as.factor)
}


#' Get element or default value from Object
#'
#' @param object object from which to get element
#' @param element_name a string matched to the names of the object
#' @param default_value return value if element not found in object
#'
#' @return element or default value
#'
#' @keywords internal
getElement2 <- function(object, element_name, default_value = NULL) {
  if (element_name %in% names(object)){
    object[[element_name]]
  } else {
    default_value
  }
}


#' Wrapper to cut values into classes using ggplot2::cut_interval() or ggplot2::cut_width(); for use with ggpolar()
#'
#' @param y object for cutting to be applied
#' @param y_cuts named list with one of the following items: list(nclass = ..., cutwidth = ...);
#' in case nclass is provided, cut_interval(y, n = y_cuts$nclass) is used for cutting, if
#' cut_width is provided, cut_width(y, width = y_cuts$cutwidth) is used
#' @param y_boundary numeric; lower boundary used for cutting
#' @param dig.lab integer; number of digits for labelling the classes derived from cutting
#'
#' @return factor, of same length as y; classes according to the used cut function
#'
#' @keywords internal
cut_fun <- function(y, y_cuts, y_boundary = NULL, dig.lab = 1, ...) {
  switch(names(y_cuts),
         nclass = cut_interval(y, n = y_cuts$nclass, dig.lab = dig.lab, ...),
         cutwidth = cut_width(y, width = y_cuts$cutwidth, boundary = y_boundary, closed = "right", ...)
  )
}


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





