ggtraj <- function(df, aesth = aes(x = lon, y = lat, group = date, color = height), incr = -seq(24,96,24),
                   lims = NULL, color_scale = scale_color_gradient(name = "m agl.")) {
  if (is.null(lims)) {lims <- list(xlim = range(df$lon, na.rm = TRUE), ylim = range(df$lat, na.rm = TRUE))}
  ggplot(df, aesth) +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "gray40", size = 0.1, fill = "gray90") +
    coord_quickmap(xlim = lims$xlim, ylim = lims$ylim) +
    geom_path() +
    geom_point(data = . %>% dplyr::filter(hour.inc %in% incr), lwd = 0.75, show.legend = FALSE) +
    geom_point(data = . %>% dplyr::filter(hour.inc == 0 & date == min(date)), shape = 21, size = 2,
               color = "white", fill = "gray20", lwd = 0.25, show.legend = FALSE) +
    geom_text_repel(data = . %>% dplyr::filter(hour.inc == min(hour.inc)), mapping = aes(label = format(date, "%H:00")),
                    color = "gray20", direction = "both", hjust = 1, vjust = 0.5, nudge_x = 0.2, nudge_y = -0.1, segment.size = 0.2, size = 2) +
    geom_text_repel(data = . %>% dplyr::filter(hour.inc == min(hour.inc)), mapping = aes(label = format(date, "%d.%m.%y")),
                    color = "gray20", direction = "both", hjust = 1, vjust = 0.5, nudge_x = 0.2, nudge_y = 0.1, segment.size = 0.2, size = 2) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_y_continuous(expand = c(0.1,0.1)) +
    scale_x_continuous(expand = c(0.1,0.1)) +
    color_scale +
    ggtitle(paste(unique(df$site), sep = ", "))
}

