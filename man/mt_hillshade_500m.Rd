% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mt_hillshade_500m.R
\docType{data}
\name{mt_hillshade_500m}
\alias{mt_hillshade_500m}
\title{A 500 Meter Hillshaded Elevation Model of Montana}
\format{
A \link[raster:Raster-classes]{RasterLayer} object.
}
\usage{
mt_hillshade_500m
}
\description{
A dataset containing a raster 500m hillshade of Montana appropriate
for state-scale mapping.
These data are derived from the 1 arc-second National Elevation Dataset,
available through \code{\link[FedData:get_ned]{FedData::get_ned()}}, aggregated to ~ 500m resolution,
and reprojected to the \emph{NAD 1983 HARN StatePlane Montana FIPS 2500} projection
coordinate system (\href{https://epsg.io/102300}{EPSG: 102300}). The data are one byte
integer values meant to represent the level of shading in a 256-shade grayscale palette.
The script that prepared the data is \url{data-raw/raw_data.R} in the source version of this
package.
}
\keyword{datasets}
