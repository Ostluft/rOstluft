library(rOstluft)


mytraj <- openair::importTraj(site = "Zurich", year = 2015:2017)
mytraj <- dplyr::filter(mytraj, lubridate::hour(date) == 12)

clusters <- openair::trajCluster(mytraj, method = "Angle", n.cluster = 6, type = "default")
clusters$data <- openair::cutData(clusters$data, type = "season")

str(clusters$data)
str(clusters$results)
table(clusters$data$cluster)

ggplot(clusters$results, aes(x = lon, y = lat, group = cluster, color = cluster)) +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "gray40", size = 0.1, fill="gray90") +
  coord_quickmap(xlim = range(clusters$results$lon), ylim = range(clusters$results$lat)) +
  geom_path(size = 1) +
  geom_point(data = dplyr::filter(clusters$results, hour.inc %in% -seq(24,96,24)), mapping = aes(color = cluster), size = 1.75, lwd = 0.75, show.legend = FALSE) +
  scale_color_brewer(palette = "YlGnBu") +
  # facet_wrap(~season) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )


df <-
  clusters$data %>%
  mutate(date = factor(as.Date(date))) %>%
  group_by(season) %>%
    dplyr::filter(date %in% sample(unique(date), 250)) %>%
  ungroup()

ggplot(df, aes(x = lon, y = lat, group = date, color = cluster)) +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "grey", size = 0.1, fill = "gray90") +
  coord_quickmap(xlim = range(df$lon), ylim = range(df$lat)) +
  geom_path(size = 0.15) +
  geom_path(data = clusters$results, color = "gray20", lwd = 0.75, show.legend = FALSE) +
  scale_color_brewer(palette = "YlGnBu") +
  facet_wrap(~season, ncol = 2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(colour = guide_legend(override.aes = list(size = 1.5)))


df <-
  bind_rows(
    clusters$data,
    clusters$data %>% mutate(season = "year")
  ) %>%
  dplyr::filter(hour.inc == 0)

ggplot(df, aes(x = 1, fill = cluster)) +
  geom_bar(stat = "count", width = 1, position = "fill") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_void() +
  facet_wrap(~season, nrow = 3) +
  coord_polar(theta = "y") +
  scale_x_continuous(limits=c(-0.75, NA)) +
  geom_text(data = . %>% dplyr::filter(!duplicated(season)), mapping = aes(x = -0.75, y = 0, label = season)) +
  theme(strip.text = element_blank())


ggplot(dplyr::filter(df, season != "year"), aes(x = 1, fill = season)) +
  geom_bar(stat = "count", width = 1) +
  scale_fill_viridis(discrete = TRUE) +
  theme_void() +
  facet_wrap(~cluster) +
  coord_polar(theta = "y") +
  scale_x_continuous(limits=c(-0.75, NA)) +
  geom_text(data = . %>% dplyr::filter(!duplicated(cluster)), mapping = aes(x = -0.75, y = 0, label = cluster)) +
  theme(strip.text = element_blank())



df <-
  clusters$data %>%
  dplyr::filter(hour.inc == 0) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date = as.Date(date), cluster) %>%
    summarise(n = n()) %>%
  group_by(date) %>%
    slice(which.max(n)) %>%
  ungroup() %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE),
    weekday = wday(date, label = TRUE), #, week_start = 1),
    monthweek = stringi::stri_datetime_fields(date, locale = "")$WeekOfMonth
  )

ggplot(df, aes(x = monthweek, y = weekday, fill = cluster)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = substring(cluster, 2, 2)), colour = "white", size = 2.5) +
  scale_fill_viridis(discrete = TRUE) +
  facet_grid(year~month) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # element_line(color = "white"),
    panel.background = element_rect(fill = "gray90", color = NA)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  xlab("Woche des Monats") +
  ylab("Wochentag") +
  ggtitle("vorherrschende Luftmassenherkunft")
