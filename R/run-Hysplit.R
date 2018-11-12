# !!! work in progress

#' berechne Monatsweise, bereinige Meteo-files chunk-wise
trajectories <-
  expand.grid(target, 3:4) %>%
  unite(sep="-", n) %>%
  dplyr::rename(months = 1) %>%
  transmute(start = as.Date(paste0(months, "-01"))) %>%
  arrange(start) %>%
  mutate(end = start + months(1) - days(1)) %>%
  rowwise() %>%
  do({

    # cat(format(c(.$start, .$end)))

    traj_temp <- hysplit_trajectory(
      run_period = as.character(c(.$start, .$end)), # c(1990, 2017), # "2017-08-02", # hier ein einzelnes Datum z.B. '2017-08-01' oder ein ganzes Jahr, z.B. '2017'
      traj_name = "ZHKaserne", # aus irgendeinem Grund crasht er, wenn hier '_' oder '-' drin ist
      lat = 47.37758,
      lon = 8.53035,
      height = 100, # Höhe agl, auf der das Luftparket losgelassen wird => 100 bedeutet, dass es eher für grossräumigen Transport repräsentativ ist
      duration = 4 * 24,
      daily_hours = c(0, 12),
      return_traj_df = TRUE, # lässt sich mit openair weiterverwenden (oder mit eigener Funktion, s.u.)
      extended_met = FALSE,
      met_type = "reanalysis", # gleich wie bei openair
      direction = "backward",
      model_height = 20000 ,
      met_dir = metdir, # entweder selber von ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/ herunterladen, oder automatisch durch splitR
      # binary_path = bindir,
      exec_dir = execdir
    ) %>%
      mutate(
        year = ifelse(year < 48, year + 2000, year + 1900) # für Kompatibilität mit openair
      )

    fs::file_delete(fs::dir_ls(metdir))
    traj_temp

  })
