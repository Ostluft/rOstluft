#' Transforms data based on an other meta table
#'
#' @description
#' To work with data from different sources a normilization of naming is necessary. This function allows flexible
#' renaming/recoding of the input data based on a second table. In short following operation is performed:
#'
#' `data$data_dest = meta[meta$meta_key == data$data_src]$meta_val`
#'
#' For handling missing values for meta_key in meta there are different modes:
#' * strict: stop execution
#' * drop: drop rows from data
#' * keep: keep the values in data
#' * replace: use the mapping provided in the argument replacement (named vector/list)
#'
#' The function is quiet chatty and reports which values are dropped, kept or replaced.
#'
#' @seealso
#' * Under the hood the heavy lifting is done by [dplyr::recode()].
#'
#' @param data Input data as tibble
#' @param meta Lookup table as tibble
#' @param data_src Name of the column in data used to lookup in meta
#' @param data_dest Name of the column to save the result in data
#' @param meta_key Name of the column in meta to match against data$data_src
#' @param meta_val Name of the column containing the replacement value in meta
#' @param mode One of "strict", "drop", "keep", "replace". Default "strict"
#' @param replacements Named vector/list with missing values in meta$meta_key or to overwrite specific mappings
#'
#' @return transformed data
#' @export
#'
#' @examples
#' meta_fn <- system.file("extdata", "meta_smn.rds",
#'                        package = "rOstluft.data", mustWork = TRUE)
#' meta <- readRDS(meta_fn)
#' tibble::glimpse(meta)
#'
#' fn <- system.file("extdata", "smn.txt", package = "rOstluft.data", mustWork = TRUE)
#' data <- read_smn(fn, na.rm = FALSE)
#' data <- dplyr::arrange(data, .data$starttime)
#' data
#'
#' # data contains no units, cryptic SwissMetNet parameter names and abbreviations for site.
#' # And the meta data for parameter rre150z0 is missing. Perfect!
#'
#' # too lazy to update meta, add unit mapping based on SwissMetNet parameter names
#' # and we want to overwrite the mapping for dkl010z0 anyway => use replace
#' res <- meta_apply(data, meta, "parameter", "unit", "parameter_original", "unit",
#'          mode = "replace",replacements = list(rre150z0 = "unit1", dkl010z0 = "unit2"))
#' res
#'
#' # rename the SwissMetNet Parameters, still no mapping for rre150z0, we aren't
#' # interested in the data and drop it
#' res2 <- meta_apply(res, meta, "parameter", "parameter",
#'          "parameter_original", "parameter", mode = "drop")
#' res2
#'
#' # or we keep it
#' res <- meta_apply(res, meta, "parameter", "parameter",
#'          "parameter_original", "parameter", mode = "keep")
#' res
#'
#' # rename the site abbreviation to the site name, strict should work
#' res <- meta_apply(res, meta, "site", "site", "site_short", "site")
#' res
#'
meta_apply <- function(data, meta, data_src, data_dest, meta_key, meta_val, mode = "strict", replacements = NULL) {
  # small helper funtion to generate message string. Thanks to closure this function sees every variable in meta_apply
  msg_str <- function(addition = "") {
    stringr::str_interp(c(
      "apply meta data$${data_dest} = meta[meta$${meta_key} == data$${data_src}]$${meta_val}:\n",
      "  missing keys in meta$${meta_key}: ${stringr::str_c(missing, collapse = ', ')}",
      addition
    ))
  }

  # more checks needed like data_src, data_dest, meta_key, meta_val are characters?
  if (!(tibble::is.tibble(data) && tibble::has_name(data, data_src) && tibble::has_name(data, data_dest))) {
    stop("data must be a tibble and has the columns %s, %s", data_src, data_dest)
  }

  if (!(tibble::is.tibble(meta) && tibble::has_name(meta, meta_key) && tibble::has_name(meta, meta_val))) {
    stop("data must be a tibble and has the columns %s, %s", meta_key, meta_val)
  }

  # get the key > value mapping from meta
  mapping <- dplyr::distinct(meta, !!sym(meta_key), !!sym(meta_val))

  mapping <- rlang::set_names(as.character(mapping[[meta_val]]), mapping[[meta_key]])
  needed <- as.character(dplyr::distinct(data, !!sym(data_src))[[data_src]])
  available <- needed %in% rlang::names2(mapping)
  missing <- needed[!available]  # names of parameter with missing mapping
  available <- needed[available] # names of parameter with available mapping

  # we could check sooner, but then we wouldn't have the missing information
  if (anyDuplicated(rlang::names2(mapping))) {
    duplicated_items <- rlang::names2(mapping)[duplicated(rlang::names2(mapping))]
    duplicated_str <- stringr::str_c(duplicated_items, collapse = ", ")  # nolint
    stop(msg_str("\n  Duplicated keys in meta$${meta_key}: ${duplicated_str}"))
  }

  if (mode == "strict" && length(missing) > 0) {
    stop(msg_str())
  } else if (mode == "drop" && length(missing) > 0) {  # only enter if keys are missing
    warning(msg_str("\n  dropping missing prameters"))
    data <- dplyr::filter(data, !!sym(data_src) %in% available)
  } else if (mode == "keep" && length(missing) > 0) {

    # that is a bit more complicated. create a source map, take the not not available in meta map from
    # the orignal source map and the available from the meta map and combine both
    src_map <- dplyr::distinct(data, !!sym(data_src), !!sym(data_dest))
    src_map <- rlang::set_names(as.character(src_map[[data_dest]]), src_map[[data_src]])
    mapping <- append(src_map[missing], mapping[available])
    warning(msg_str("\n  keeping values: ${stringr::str_c(src_map[missing], collapse = ', ')}"))
  } else if (mode == "replace" && length(missing) > 0) {

    # check all missing keys are in the replacements
    if (!all(missing %in% rlang::names2(replacements))) {
      missing_repl <- missing[!(missing %in% rlang::names2(replacements))]  # nolint
      stop(msg_str("\n  missing names in replacements: ${stringr::str_c(missing_repl, collapse = ', ')}"))
    }

    # check for duplicated names in replacements
    names_repl <- rlang::names2(replacements)
    if (anyDuplicated(names_repl)) {
      duplicated_items <- names_repl[duplicated(names_repl)]
      duplicated_str <- stringr::str_c(duplicated_items, collapse = ", ")
      stop(msg_str("\n  Duplicated names in replacements: ${duplicated_str}"))
    }

    # we give keys in replacements the higher prio than keys in meta -> need to rebuild selection vectors
    in_repl <- needed %in% names_repl
    in_map <-  !in_repl & (needed %in% rlang::names2(mapping))
    in_repl <- needed[in_repl] # names in replacements
    in_map <- needed[in_map]   # names in mapping
    mapping <- append(replacements[in_repl], mapping[in_map])
    message(msg_str("\n  replacements used: ${stringr::str_c(names(replacements[in_repl]), collapse = ', ')}"))
  }

  # we should have checked everything and built an appropriate mapping
  data <- dplyr::mutate(data, !!sym(data_dest) := dplyr::recode(data[[data_src]], !!!mapping))
}
