


#' various polar plots with ggplot2
#'
#' calculates summary statistics and creates polar plots from those; usually such polar plots are used
#' for data containing wind direction information, e.g. wind-roses, concentration-roses, advection-roses etc
#'
#' @param df data.frame including wind data
#' @param wd string; giving column name for the wind direction (in °)
#' @param y string; giving column name for the parameter supplied to the summary statistics (e.g. wind speed, NOx concentration etc)
#' @param z string, optional => can be NULL; parameter supplied to the summary statistics (e.g. NOx concentration);
#' only used when geom == "heat" (result will be displayed as color scale)
#' @param stat string; the summary statistics for use in stat_summary() (e.g. "count", "mean", "sd", "quantile" etc)
#' @param geom string; the type geom for plotting (one of "bar", "radar", "heat")
#' @param group string, optional => can be NULL; giving column name for a grouping variable
#' @param wd_cutwidth width of wind direction classes
#' @param y_cuts named list with one of list(nclass = ..., cutwidth = ...); ... is a single number,
#' either for the number of cuts applied on y (nclass) or the cut-width applied on y (cutwidth)
#' @param y_boundary numeric, optional => can be NULL; lower boundary used for cutting
#' @param y_cap numeric, optional => can be Inf; upper limit for cutting
#' @param dig.lab integer; number of digits for labelling the classes derived from cutting
#' @param border_bar string; color used for border of geom_bar
#' @param radar_opacity numeric; opacity for the fill color in geom = "radar"
#' @param ...; other parameters supplied to stat_summary(), e.g. fun.args = list(na.rm = TRUE)
#'
#' @return ggplot2-graph object (=> can be further modified subsequently, e.g. overwriting scale_colour or applying facetting)
#'
#' @keywords plotting
ggHysplit_traj <- function(df) {
  ggplot(df, aes(x = lon, y = lat, group = date, color = height)) +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "gray40", size = 0.1, fill = "gray90") +
    coord_quickmap(xlim = range(df$lon), ylim = range(df$lat)) +
    geom_path() +
    geom_point(data = dplyr::filter(df, hour.inc %in% -seq(24,96,24)), lwd = 0.75, show.legend = FALSE) +
    geom_text_repel(data = dplyr::filter(df, hour.inc %in% -96), mapping = aes(label = format(date, "%H:00")), direction = "both",
                    hjust = 1, vjust = 0.5, nudge_x = 0.2, nudge_y = 0.1, segment.size = 0.2, size = 3) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_y_continuous(expand = c(0.1,0.1)) +
    scale_x_continuous(expand = c(0.1,0.1)) +
    scale_color_gradient(name = "Höhe, magl")
}


# Bsp
# library(rOstluft)
# library(ggedit)
# mytraj <- importTraj(site = "london", year = 2009)
# df_traj <- dplyr::filter(mytraj, as.Date(date) == as.Date("2009-08-01"))
# ggHysplit_traj(df_traj) + ggtitle(format(unique(df_traj$date), "%d.%m.%Y"))
#
# df_traj <-
#   mytraj %>%
#   dplyr::filter(as.Date(date) %in% (as.Date("2009-08-01") + lubridate::days(0:6))) %>%
#   mutate(
#     date3 = factor(as.Date(date)),
#     hour2 = format(date, "%H:%M")
#   )
#
# ggHysplit_traj(df_traj) +
#   facet_wrap(date3~.)
#
# ggHysplit_traj(dplyr::filter(df_traj, hour2 == "12:00")) +
#   aes(color = date3)+
#   scale_color_brewer(name = "Tag", palette = "Set1")
#
# p <- ggHysplit_traj(df_traj) +
#   aes(color = date3) +
#   scale_color_brewer(name = "Tag", palette = "Set1") +
#   facet_wrap(hour2~.)
#
# p %>% ggedit::remove_geom("text")



