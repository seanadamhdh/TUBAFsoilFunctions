% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Spectrolyzer_load.R
\name{rename_flags}
\alias{rename_flags}
\title{Rename flag columns}
\usage{
rename_flags(parameter_df)
}
\arguments{
\item{paramter_df}{A raw parameter file loaded with read_csv(...)}
}
\value{
Tibble with colnames changed to ..., var1, Flags_var1, var2, Flags_var2, ...
}
\description{
Helper function for renaming Flag columns in spectrolyzer parameter files. Intended for internal use.
Colnames should be structured as ..., var1, Flags...n, var2, Flags...n, ... where n is the column index automatically assigned by read_csv
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
