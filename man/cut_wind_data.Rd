% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{cut_wind_data}
\alias{cut_wind_data}
\title{Cut wind data}
\usage{
cut_wind_data(data, wind_parameters = c("WD", "WVv"))
}
\arguments{
\item{data}{tibble with input data}

\item{wind_parameters}{Character Vector of Wind parameters. Default wind_parameters = c("WD", "WVv")}
}
\value{
named list: $wind all wind parameters, $others everything else
}
\description{
This function splits off the wind parameters
}
\details{
TODO: create an all purpuse function?
}
\keyword{internal}
