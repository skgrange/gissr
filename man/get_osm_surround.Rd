% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_osm_surround.R
\name{get_osm_surround}
\alias{get_osm_surround}
\title{Function to get points, lines, or polygons from OpenStreetMap using 
\code{ra.osmsurround.org}.}
\usage{
get_osm_surround(id, type = "data", progress = "none")
}
\arguments{
\item{id}{A vector of OpenStreetMap IDs.}

\item{type}{Type of data to return. Can be one of \code{"points"}, 
\code{"lines"}, \code{"polygons"}, or \code{"data"}. If \code{"data"},
a data frame will be returned rather than spatial data.}

\item{progress}{Type of progress bar to display.}
}
\value{
Spatial data or data frame.
}
\description{
Function to get points, lines, or polygons from OpenStreetMap using 
\code{ra.osmsurround.org}.
}
\examples{
\dontrun{

# Get a walking route in York as spatial lines
sp_walking_route <- get_osm_surround(1069837, type = "lines")

# Get some polygons for New York
sp_new_york <- get_osm_surround(c(369519, 369518, 3954665), "polygons")

# Plot
leaflet_plot(sp_new_york)

}

}
\author{
Stuart K. Grange
}
