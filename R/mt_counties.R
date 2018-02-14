#' Polygons of Montana Counties
#'
#' A dataset containing polygons of Montana counties.
#' These data are built from the Montana Cadastral database,
#' and are available from [the Montana State Library](http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaCounties.zip),
#' and transformed using [sf::st_transform()] to the *NAD 1983 HARN StatePlane Montana FIPS 2500* projection
#' coordinate system ([EPSG: 102300](https://epsg.io/102300)). They were then joined with the [Montana
#' Climate Divisions](https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ncdc:C00005).
#' These polygons are appropriate for large print maps.
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A simple feature collection with 56 features and 8 fields:
#' * **County** — the county name
#' * **State FIPS code** — the Federal Information Processing Standard (FIPS) state code
#' * **County FIPS code** — the Federal Information Processing Standard (FIPS) county code
#' * **Division** — the NOAA Climate Divisional Database (nCLIMDIV) climate division name
#' * **Division code** — the NOAA Climate Divisional Database (nCLIMDIV) identifier
#' * **Division FIPS code** — the Federal Information Processing Standard (FIPS) climate division code
#'
#' @source \url{http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaCounties.zip}
"mt_counties"
