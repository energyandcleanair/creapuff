% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_calpuff_files}
\alias{get_calpuff_files}
\title{Build concentration (additional) dataset from CALPUF results}
\usage{
get_calpuff_files(ext = ".csv", gasunit = "ug", dir = ".", hg_scaling = 1)
}
\arguments{
\item{gasunit}{are gas concentrations desired in ug or ppb (CALPOST outputs are assumed to be in ug/m3)}

\item{utm_hem}{'N' or 'S'}

\item{map_res}{in kilometers}
}
\description{
Build concentration (additional) dataset from CALPUF results
}
