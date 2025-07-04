% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Spectrolyzer_load.R
\name{Spectro_merge_col}
\alias{Spectro_merge_col}
\title{Merge fingerprint and parameter data}
\usage{
Spectro_merge_col(fingerprint, parameter)
}
\arguments{
\item{fingerprint}{fingerprint tibble, created by Spectro_load_fingerprints}

\item{parameters}{fingerprint tibble, created by Spectro_load_parameters}
}
\value{
Tibble with merged fingerprint and parameter data.
}
\description{
Merging of fingerprint and parameter files loaded with Spectro_load_fingerprint and Spectro_load_parameter. Intended primarily for internal use.
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\note{
Warning messages: "Unknown or initialised colum: \code{Serial_No} ...
known warning, function switches automatically to alternate case.
}
