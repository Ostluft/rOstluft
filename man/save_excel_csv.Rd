% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_excel_csv.R
\name{save_excel_csv}
\alias{save_excel_csv}
\title{Speichert Datein in einer Excel kompatiblen CSV Datei}
\usage{
save_excel_csv(data, fn)
}
\arguments{
\item{data}{Zu speichernde Daten}

\item{fn}{Pfad zur Datei}
}
\value{
invisible(NULL)
}
\description{
Für eine Region Schweiz Excel kompatible Datei zu erstellen müssen folgende Bedingungen erfüllt sein:
\itemize{
\item UTF-8 mit Byte Order Mark (UTF-8 BOM)
\item Delimiter ist ";"
\item Dezimalpunkt ist "."
\item NA Werte sind "#NV"
\item Datumsformat ist "\%d.\%m.\%Y \%H:\%M:\%S"
}

Diese Funktion kümmert sich um all diese Kleinigkeiten. Zudem konventiert sie alle Zeitangeben als \code{\link[=POSIXct]{POSIXct()}} oder
\code{\link[=POSIXlt]{POSIXlt()}} mit Hilfe von \code{\link[=format.POSIXct]{format.POSIXct()}}, bzw. \code{\link[=format.POSIXlt]{format.POSIXlt()}} unter berücksichtung des Arguments \code{tz} in
Characters vor dem Aufruf von \code{\link[readr:write_delim]{readr::write_excel_csv()}}.
}
\seealso{
\itemize{
\item \code{\link[readr:write_delim]{readr::write_excel_csv()}}
}
}
