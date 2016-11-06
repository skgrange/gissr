% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp_fortify.R
\name{sp_fortify}
\alias{sp_fortify}
\title{Function to wrap ggplot2::fortify which transforms geometries to data frames
(tables).}
\usage{
sp_fortify(sp, rename = TRUE)
}
\arguments{
\item{sp}{Spatial object.}

\item{rename}{Should \code{lat} and \code{long} be renamed to \code{latitude} 
and \code{longitude}? Default is \code{TRUE}.}
}
\description{
Function to wrap ggplot2::fortify which transforms geometries to data frames
(tables).
}
\author{
Stuart K. Grange
}
\seealso{
\code{\link{fortify}}
}
