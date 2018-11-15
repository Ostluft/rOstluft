# gleich wie bei den meteo schweiz

# bottom up ansatz

# daten aus einer datei lesen




read_ethz_iac <- function(file, encoding = "latin1", tz = "Etc/GMT-1", time_shift = NULL) {
  # site information is in the file, could parse from the header or provide it as argument
  header <- readr::read_lines(file, n_max = 10L)

  # invalid values include "99999.0", "9999.0", "-320" <- note sure how it is formatted
  data <- readr::read_table(file, skip = 10L) # needs some more parameter :P
}


get_ethz_iac_con <- function(date, site, userpw = NULL, cache = NULL) {

}
