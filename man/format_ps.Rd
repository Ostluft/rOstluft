% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format-ps.R
\docType{class}
\name{format_ps}
\alias{format_ps}
\title{Format defintion for ps data with endtime instead of interval}
\usage{
format_ps(tz = "Etc/GMT-1")
}
\arguments{
\item{tz}{time zone used for chunking/content. Default Etc/GMT-1}
}
\value{
R6 class object of format_rolf
}
\description{
This format is used for non periodical data
}
\section{TODO}{

Define class of columns
}

\section{Columns}{

\itemize{
\item \strong{starttime}: POSIXct
\item \strong{endtime}: POSIXct
\item \strong{site}: factor/character
\item \strong{parameter}: factor/character
\item \strong{unit}: factor/character
\item \strong{value}: double
}
}

\section{Series}{


A serie is the unique combination of the columns:
\itemize{
\item site
\item parameter
\item unit
}
}

\section{Chunking}{


The data is chunked by the columns
\itemize{
\item site
}
}

\section{Content Columns}{

\itemize{
\item year = year(with_tz(starttime, tz))
\item site
\item parameter
\item unit
\item n = number of valid values
}
}

