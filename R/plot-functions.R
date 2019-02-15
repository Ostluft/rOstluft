#' provides the coordinate geom for plotting a radar polar plot in ggplot2; based on ggproto-package
#'
#'
#' @return ggplot2 coordinate-geom
#'
#' @keywords internal
#' from here: https://medium.com/@rhdzmota/alcohol-and-radar-plots-in-r-with-ggplot2-9ba7ad8c92c
coord_radar <- function (theta = "x", start = 0, direction = 1, ...) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), is_linear = function(coord) TRUE, ...)
}


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
ggpolar <- function(df, wd, y, z = NULL, stat = "count", geom = "bar", group = NULL, wd_cutwidth = 20, y_cuts = list(nclass = 5),
                    y_boundary = 0, y_cap = Inf, dig.lab = 1, border_bar = "white", radar_opacity = 0.2, ...) {

  #' hier noch Tests und stop messages einbauen...
  #' geom == "heat" noch fertig stellen...

  df <- df %>%
    dplyr::mutate(
      x_classes = cut_width(!!sym(wd), width = wd_cutwidth, closed = "left", boundary = 0),
      y_classes = cut_fun(pmin(!!sym(y), y_cap, na.rm = TRUE), y_cuts = y_cuts, y_boundary = y_boundary, dig.lab = dig.lab)
    ) %>%
    dplyr::filter(!is.na(x_classes) & !is.na(y_classes) & !is.na(!!sym(y))) %>%
    mutate_at(group, as.factor)

  if (!is.infinite(y_cap)) {
    maxlevel <- tail(unlist(strsplit(levels(df$y_classes), split = ",", fixed = TRUE)), 2)[1]
    levels(df$y_classes)[length(levels(df$y_classes))] <- paste0(">", substring(maxlevel, 2, nchar(maxlevel)))
  }

  fill_name <- y
  opacity <- 1
  discr <- TRUE
  pos <- "stack"
  if (stat == "count") {stat <- "length"}
  if (stat != "length" | geom == "radar") {pos <- "identity"}
  aesthetics <- aes_string(x = "x_classes", y = y, fill = "y_classes", color = "y_classes")
  coord <- coord_polar()
  scale_x <- scale_x_discrete(expand = c(0, 0))
  scale_y <- scale_y_continuous(expand = c(0.05, 0), breaks = function(x) pretty(x, n = 4), minor_breaks = NULL)

  if (!is.null(group)) {
    opacity = 0.5
    fill_name <- group
    aesthetics <- aes_string(x = "x_classes", y = y, group = group, fill = group)
    scale_y <- scale_y_discrete(expand = c(0.05, 0), breaks = function(x) pretty(x, n = 4))
  } else if (is.factor(df[1,y]) & is.null(group)) {
    opacity = 0.5
    aesthetics <- aes_string(x = "x_classes", y = y, fill = y, color = y)
  }
  if (geom == "bar" & !is.null(group)) {pos <- "identity"}
  geom_plot <- stat_summary(fun.y = stat, geom = "bar", width = 1, position = pos, alpha = opacity, color = border_bar, size = 0.1, ...)
  if (geom == "radar") {
    if (is.null(group)) {group_radar <- "y_classes"} else {group_radar <- group}
    coord <- coord_radar()
    aesthetics <- aes_string(x = "x_classes", y = y, group = group_radar, fill = group_radar, color = group_radar)
    geom_plot <- stat_summary(fun.y = stat, geom = "polygon", alpha = radar_opacity, ...)
    scale_x <- scale_x_discrete()
    scale_y <- scale_y_continuous(expand = c(0, 0), breaks = function(x) pretty(x, n = 4), minor_breaks = NULL)
  }  else if (geom == "heat") {
    aesthetics <- aes_string(x = "x_classes", y = "y_classes", group = "y_classes", fill = z)
    geom_plot <- stat_summary(fun.y = stat, geom = "tile", ...)
    scale_y <- scale_y_discrete(expand = c(0.05, 0), breaks = function(x) pretty(x, n = 4))
  }
  if (stat != "length" & is.null(group) & !is.factor(df[1,y]) & geom != "radar") {
    aesthetics <- aes_string(x = "x_classes", y = y, fill = "..y..", color = "..y..")
    discr <- FALSE
  }

  ggplot(df) +
    aesthetics +
    geom_plot +
    scale_x +
    scale_y +
    coord +
    viridis::scale_color_viridis(name = fill_name, option = "E", discrete = discr, direction = -1) +
    viridis::scale_fill_viridis(name = fill_name, option = "E", discrete = discr, direction = -1) +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    )

}


# Bsp
#' library(rOstluft)
#' library(rOstluft.data)
#' files <- list.files(system.file("extdata", package = "rOstluft.data"))
#' df <- read_airmo_csv(list.files(system.file("extdata", package = "rOstluft.data"), full.names = TRUE)[files == "Zch_Stampfenbachstrasse_2010-2014.csv"]) %>%
#'   dplyr::select(-einheit, -station, -intervall) %>%
#'   spread(parameter, wert) %>%
#'   dplyr::rename(date = startzeit) %>%
#'   openair::cutData(type = "daylight") %>%
#'   openair::cutData(type = c("hour", "season"))
#' str(df)
#'
#' #' Varianten von Windverteilungen
#' ggpolar(df, wd = "WD", y = "WVv", stat = "count", group = NULL, y_cap = 3.5, y_cuts = list(cutwidth = 0.5))
#' ggpolar(df, wd = "WD", y = "WVv", y_cap = 3.5, y_cuts = list(cutwidth = 0.5)) + scale_fill_brewer(name = "WVv", palette = "RdYlBu", direction = -1)
#' ggpolar(df, wd = "WD", y = "WVv", y_cap = 4.5, y_cuts = list(nclass = 6), border_bar = NA) + facet_grid(season~daylight, switch = "y")
#' ggpolar(df, wd = "WD", y = "WVv", y_cap = 4.5, y_cuts = list(cutwidth = 1), border_bar = NA) + facet_wrap(season~., ncol = 2)
#' ggpolar(df, wd = "WD", y = "WVv", y_cap = 4.5, y_cuts = list(cutwidth = 1), border_bar = NA) + facet_wrap(hour~., ncol = 6) + theme(panel.grid = element_blank())
#' ggpolar(df, wd = "WD", y = "WVv", stat = "max", fun.args = list(na.rm = TRUE)) + theme_bw()
#' ggpolar(df, wd = "WD", y = "WVv", stat = "median") + theme(panel.grid = element_blank())
#' ggpolar(df, wd = "WD", y = "WVv", stat = "median") + theme(panel.grid.major.x = element_line())
#' ggpolar(df, wd = "WD", y = "WVv", stat = "sd")
#' ggpolar(df, wd = "WD", y = "WVv", group = "daylight", stat = "quantile", fun.args = list(na.rm = TRUE, probs = 0.9))
#' ggpolar(df, wd = "WD", y = "WVv", wd_cutwidth = 10, geom = "radar", stat = "mean", group = "daylight")
#' ggpolar(df, wd = "WD", y = "WVv", wd_cutwidth = 10, geom = "radar", stat = "count", group = NULL, y_cap = 3.5, y_cuts = list(cutwidth = 1), radar_opacity = 0) + theme(panel.grid = element_blank())
#'
#' #' Schadstoff-Verteilung anstatt Windstärke
#' ggpolar(df, wd = "WD", y = "NO2", y_cap = 75, y_cuts = list(cutwidth = 10), border_bar = NA) + theme(panel.grid = element_blank())
#' ggpolar(df, wd = "WD", y = "NO2", y_cap = 75, y_cuts = list(nclass = 6), border_bar = NA) + facet_grid(season~daylight, switch = "y") + theme(panel.grid = element_blank())
#'
#' #' Quellenbezüge werden mit Fracht-Rosen unter Umstaenden besser hervorgehoben
#' ggpolar(mutate(df, PM10_Fracht = WVv * pmax(0, PM10)), wd = "WD", y = "PM10_Fracht", y_cap = 71, y_cuts = list(cutwidth = 10), border_bar = NA) + theme(panel.grid = element_blank())
#' ggpolar(mutate(df, PM10_Fracht = WVv * pmax(0, PM10)), wd = "WD", y = "PM10_Fracht", wd_cutwidth = 10, geom = "radar", stat = "quantile", fun.args = list(na.rm = TRUE, probs = 0.75), group = "season", radar_opacity = 0) + theme(axis.text.y = element_text(), axis.ticks.y = element_line()) + scale_color_viridis(discrete = TRUE)
#'
#' #' Plotte Windrose auf Kartenausschnitt
#' #' ...kommt noch




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







