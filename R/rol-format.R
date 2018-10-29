# idea
# initiate store:
# store <- create_Store(driver_rds(format_rol()))
# store$put(data)
put <- function(data) {

  # format definitions
  serie_columns <- c("interval", "site", "parameter", "unit")
  chunk_nest <- list("year" = ~lubridate::year(starttime))
  chunk_columns <- c("interval", "site")


  df <- dplyr::group_by(data, .dots = chunk_nest)
  df <- tidyr::nest(df, .key = groups)
  df <- dplyr::mutate(df, groups = purrr::pmap(df, do_col_grouping, chunk_columns, serie_columns))
  tidyr::unnest(df, groups)
}


do_col_grouping <- function(groups, chunk_columns, serie_columns, ...) {
  groups <- dplyr::group_by(groups, .dots=chunk_columns)
  res <- dplyr::do(groups, do_merge(.data, chunk_columns, serie_columns, ...))
  res
}


# store driver specific
do_merge <- function(group, chunk_columns, serie_columns, ...) {
  chunk_labels <- dplyr::summarise_at(group, dplyr::first, .vars = chunk_columns)
  args <- c(..., as.list(chunk_labels))
  args <- purrr::map(args, as.character)
  chunk_path <- purrr::invoke(format_get_chunk_path, .x=args)

  if (fs::file_exists(chunk_path)) {
    dfo <- readRDS(chunk_path)
    dfn <- format_merge(group, dfo, serie_columns)
  } else {
    fs::dir_create(fs::path_dir(chunk_path))
    dfn <- group
  }
  dfn <- droplevels(dfn)
  dfn <- format_sort(dfn)
  saveRDS(dfn, chunk_path)
  dplyr::count(dfn, .dots=serie_columns)
}



format_get_chunk_path <- function(interval, site, year) {
  fn <- base64url::base64_urlencode(paste(interval, site, year, sep = "»"))
  fs::path_join(c("c:/test", interval, paste0(fn, ".rds")))
}

format_merge <- function(x, y, serie_columns) {
  xy <- bind_rows_with_factor_columns(y, x)
  dplyr::distinct(xy, starttime, interval, site, parameter, unit, .keep_all = TRUE)
}

format_sort <- function(x) {
  dplyr::arrange(x, .data$starttime)
}

