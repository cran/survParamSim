% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_pi.R
\name{extract_km_pi}
\alias{extract_km_pi}
\alias{extract_median_surv}
\alias{extract_hr_pi}
\title{Functions to extract prediction intervals and observed data}
\usage{
extract_km_pi(km.pi, trunc.sim.censor = TRUE)

extract_median_surv(km.pi, outtype = c("long", "wide"))

extract_hr_pi(hr.pi, outtype = c("long", "wide"))
}
\arguments{
\item{km.pi}{A return object from \code{\link{calc_km_pi}} function.}

\item{trunc.sim.censor}{A logical specifying whether to truncate the simulated
curve at the last time of `censor.dur`` specified in \code{\link{surv_param_sim}}.}

\item{outtype}{Specifies whether output will be in long or wide format.}

\item{hr.pi}{a return object from \code{\link{calc_hr_pi}} function.}
}
\description{
Functions to extract prediction intervals and observed data
}
\details{
\code{\link{extract_km_pi}} extracts prediction intervals of simulated Kaplan-Meier curves.

\code{\link{extract_median_surv}} extracts prediction intervals of
median survival times and and the corresponding observed values.

\code{\link{extract_hr_pi}} extracts prediction intervals of simulated
hazard ratios and the corresponding observed values.
}
