% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SEMACH_v3_readR.R
\name{SEMACHv3_calib}
\alias{SEMACHv3_calib}
\title{SEMACHv3 SCD30 Calibration}
\usage{
SEMACHv3_calib(
  dataset = NULL,
  dir_path = NULL,
  cutoff_start = 0,
  cutoff_end = 99999,
  plot = F,
  cal = T,
  min_nrow = 100
)
}
\arguments{
\item{dataset}{A single or multiple glued (i.e. bind_rows()) raw SMEACHv3 dataset loaded with \code{load_SEMACHv3_raw} (or equivalent). Must contain SCD30 and GMP252 data for at least a part of the dataset.}

\item{dir_path}{Path to folder containing subfolders with measurements (i.e. several days), or directly to a folder containing measurements}

\item{cutoff_start}{Of the individual measurements, the first seconds that should be omitted}

\item{cutoff_end}{Only use measurement data up to this time}

\item{plot}{Boolean. Should a calibration plot be returned? At least one of \code{plot} and \code{cal} should be True}

\item{cal}{Boolean. Should a result table of the calibrations be returned?}

\item{min_nrow}{Minimum number of datapoints for calculation}
}
\description{
Calculation of slope + offset fro linear post-hoc calibration / correction of SEMACHv3 SCD30 data using GMP252 data
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
