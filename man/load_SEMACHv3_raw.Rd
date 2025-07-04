% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SEMACH_v3_readR.R
\name{load_SEMACHv3_raw}
\alias{load_SEMACHv3_raw}
\title{laod_SEMACHv3_raw}
\usage{
load_SEMACHv3_raw(path, version = "v3.2")
}
\arguments{
\item{path}{A file path to a SEMACHv3 .csv measurement file.}

\item{version}{SEMACH version for compatability with header read in. Default v3.2 (most recent systems)}
}
\value{
Formatted raw SEMACHv3 measurement files as a tibble. File name is used as ID.
}
\description{
A function to load one or multiple SEMACHv3 raw files and add them to a tibble. Useful for displaying CO2 slopes or sensor calibrations.
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
