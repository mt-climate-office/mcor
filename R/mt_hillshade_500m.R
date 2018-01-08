#' A 500 Meter Hillshaded Elevation Model of Montana
#'
#' A dataset containing a raster 500m hillshade of Montana appropriate
#' for state-scale mapping.
#' These data are derived from the 1 arc-second National Elevation Dataset,
#' available through [FedData::get_ned()], aggregated to ~ 500m resolution,
#' and reprojected to the *NAD 1983 HARN StatePlane Montana FIPS 2500* projection
#' coordinate system ([EPSG: 102300](https://epsg.io/102300)). The data are one byte
#' integer values meant to represent the level of shading in a 256-shade grayscale palette.
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A [RasterLayer][raster::Raster-class] object.
"mt_hillshade_500m"
