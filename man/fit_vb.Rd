% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vb.R
\name{fit_vb}
\alias{fit_vb}
\title{Fit von Bertalanffy curve}
\usage{
fit_vb(age, length, ...)
}
\arguments{
\item{age}{A numeric vector of ages}

\item{length}{A numeric vector of lengths}

\item{...}{Other arguments to pass to \code{\link[rstan]{sampling}}.}
}
\description{
Fit a von Bertalanffy growth curve to fish age and length of data to generate
posterior samples of age-length parameters to feed into an operating model
simulation.
}
\examples{
m <- fit_vb(short_age_length$age, short_age_length$length)
m
}
\seealso{
\code{\link{get_age_length}}
}
