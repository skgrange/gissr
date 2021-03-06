% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ra_values.R
\name{ra_values}
\alias{ra_values}
\title{Function to return all values in a raster object.}
\usage{
ra_values(ra)
}
\arguments{
\item{ra}{A raster object.}
}
\value{
Numeric vector or numeric matrix.
}
\description{
Function to return all values in a raster object.
}
\examples{

# Load raster object
ra_example <- raster::raster(system.file("external/test.grd", package = "raster"))

# Get values
ra_values(ra_example)

# Get unique values and sort
sort(unique(ra_values(ra_example)))

}
\seealso{
\code{\link[raster]{values}}
}
\author{
Stuart K. Grange
}
