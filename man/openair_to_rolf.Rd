% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openair.R
\name{openair_to_rolf}
\alias{openair_to_rolf}
\title{Converts data from openair format to rolf}
\usage{
openair_to_rolf(
  data,
  site = NULL,
  interval = NULL,
  units = NULL,
  ws = "WVv",
  wd = "WD"
)
}
\arguments{
\item{data}{in openair format}

\item{site}{specify site of the data. Necessary if data contains no column "site"}

\item{interval}{specify interval of the data. Necessary if data contains no column "interval"}

\item{units}{specify parameter unit mapping as named vector. Name = parameter, value = unit. Necessary if data
contains no column "unit"}

\item{ws}{renames parameter "ws" to this Value. Default "WVv". Set ws = NULL to disable renaming}

\item{wd}{renames parameter "wd" to this Value. Default "WD". Set wd = NULL to disable renaming}
}
\value{
Converted data in rolf format
}
\description{
Converts data from openair format to rolf
}
\examples{
h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv",
                   package = "rOstluft.data", mustWork = TRUE)

airmo_h1 <- read_airmo_csv(h1)
airmo_h1

oa_h1 <- rolf_to_openair(airmo_h1)
units_h1 <- attr(oa_h1, "units")

# we lost the parameter with ppb/ppm units trough the convertions
openair_to_rolf(oa_h1, interval = "h1", units = units_h1)

}
