#' Polygons of Montana Tribal Land
#'
#' A dataset containing polygons of Montana Tribal Lands.
#' These data are available from [the Montana State Library](http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaReservations.zip),
#' and transformed using [sf::st_transform()] to the *NAD 1983 HARN StatePlane Montana FIPS 2500* projection
#' coordinate system ([EPSG: 102300](https://epsg.io/102300)).
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A simple feature collection with 7 features and 1 field:
#' * **Name** — the reservation name
#'
#' @source \url{http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaReservations.zip}
"mt_tribal_land"
