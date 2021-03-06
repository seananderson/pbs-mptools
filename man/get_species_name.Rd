% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup.R
\name{get_species_name}
\alias{get_species_name}
\title{Get a species name given a species ID}
\usage{
get_species_name(species_id = NULL, con = db_connection())
}
\arguments{
\item{species_id}{A species ID code}

\item{con}{A connection object to a database}
}
\description{
Get a species name given a species ID
}
\examples{
\dontrun{
get_species_name(403)
head(get_species_name())
}
}
