% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp_promote.R
\name{sp_promote}
\alias{sp_promote}
\title{Function to add a data slot in geometries.}
\usage{
sp_promote(sp)
}
\arguments{
\item{sp}{Spatial object}
}
\description{
\code{sp_promote} will add a single variable named \code{id} to the data slot
of a geometry. \code{id} is a character vector.
}
\examples{
\dontrun{

# Promote to spatial data frame
sp_with_data <- sp_promote(sp)

}

}
\author{
Stuart K. Grange
}
