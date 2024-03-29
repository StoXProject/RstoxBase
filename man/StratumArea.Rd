% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Spatial.R
\name{StratumArea}
\alias{StratumArea}
\title{Calculate area of each stratum}
\usage{
StratumArea(StratumPolygon, AreaMethod = c("Accurate", "Simple"))
}
\arguments{
\item{StratumPolygon}{The \code{\link{StratumPolygon}} process data.}

\item{AreaMethod}{The method to use for the area calculation, defaulted to "Accurate", which applied a Lambert azimuthal equal area projection.}
}
\value{
An object of StoX data type \code{\link{StratumAreaData}}.
}
\description{
This function calculated the area of each stratum.
}
\details{
The area output is given in international square nautical miles. 

The \code{AreaMethod} "Accurate" calculates each stratum are using the function \code{\link[sf]{st_area}} with the stratum transformed to Cartesian coordinates using the WGS84 Lambert Azimuthal Equal Area projection with origin at the centroid of the stratum as calculated from the geographical coordinates (longitude, latitude).

The \code{AreaMethod} "Simple" is used in StoX 2.7 and earlier versions and kept for backwards compatibility.
}
\seealso{
\code{\link{DefineStratumPolygon}} for the \code{StratumPolygon} input to the function.
}
