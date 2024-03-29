% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{bind_rows_with_factor_columns}
\alias{bind_rows_with_factor_columns}
\title{bind rows of data frames with factors}
\usage{
bind_rows_with_factor_columns(...)
}
\arguments{
\item{...}{data frames to combine}
}
\value{
combined data frame
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

Before dplyr 1.0.0 \code{\link[dplyr:bind_rows]{dplyr::bind_rows()}} coerced factors with different levels
to characters. This is no longer the case and this function is deprecated

\emph{Important}: It is mandatory that all data frames have the same structure
}
\keyword{internal}
