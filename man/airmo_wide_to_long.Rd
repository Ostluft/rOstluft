% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-airmo.R
\name{airmo_wide_to_long}
\alias{airmo_wide_to_long}
\title{Converts the wide format from the files to the long format}
\usage{
airmo_wide_to_long(
  header,
  data,
  tz = "Etc/GMT-1",
  time_shift = NULL,
  na.rm = TRUE
)
}
\arguments{
\item{header}{data frame containing airmo_kurzname, parameter, zeitfenster and einheit}

\item{data}{data frame only containing the data from the file}

\item{tz}{time zone of date field. Be carefull Etc/GMT + == -. Default "Etc/GMT-1"}

\item{time_shift}{a lubridate period to add to the time. Default NULL}

\item{na.rm}{remove na (empty) values. Default TRUE}
}
\value{
data frame in rOstluft long format
}
\description{
Converts the wide format from the files to the long format
}
\seealso{
\code{\link[lubridate:period]{lubridate::period()}} - Create or parse period objects

\link[base:timezones]{base::timezones} - Information about time zones in R
}
\keyword{internal}
