% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SEMACH_v3_readR.R
\name{eval_SEMACHv3}
\alias{eval_SEMACHv3}
\title{eval_SEMACHv3}
\usage{
eval_SEMACHv3(
  path,
  height_offset = 0,
  SCD30_scaling = 1.129,
  SCD30_offset = 23.953,
  cutoff_start = 61,
  cutoff_end = 3600
)
}
\arguments{
\item{path}{A file path to a SEMACHv3 .csv measurement file.}

\item{height_offset}{Additional chamber volume will be calculated from extra height in m.}

\item{SCD30_scaling}{Scaling factor a for the SCD30 CO2 sensor for post-hoc calibration. (CO2.corr=a*CO2.SCD30+b)}

\item{SCD30_offset}{Scaling offset b for the SCD30 CO2 sensor for post-hoc calibration. (CO2.corr=a*CO2.SCD30+b)}

\item{cutoff_start}{The first n seconds of the measurement will be disregarded (equillibration period, 60s recommended).}

\item{cutoff_end}{Measurement will only be used to this point (in s). Long measurements might have non-linear behaviour.}
}
\value{
Formatted raw SEMACHv3 measurement file
}
\description{
A function for the calculation of respiration fluxes from SEMACH files.
}
