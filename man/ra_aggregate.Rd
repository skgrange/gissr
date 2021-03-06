% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ra_aggregate.R
\name{ra_aggregate}
\alias{ra_aggregate}
\title{Function to create a lower resolution (aggregated) raster layer.}
\usage{
ra_aggregate(ra, factor, fun = mean, na.rm = TRUE)
}
\arguments{
\item{ra}{Raster object.}

\item{factor}{Amount of aggregatation in number of cells or as a multiplier 
to decrease \code{ra}'s current resolution.}

\item{fun}{Summary function to for aggregatation.}

\item{na.rm}{Should \code{NA}s be omited from the aggregatation?}
}
\value{
Raster object.
}
\description{
Function to create a lower resolution (aggregated) raster layer.
}
\examples{
\dontrun{

# Decrease a raster layer's resolution by five times
ra_countries_rough <- ra_aggregate(ra_countries, factor = 5)

}

}
\seealso{
\code{\link{ra_disaggregate}}
}
\author{
Stuart K. Grange
}
