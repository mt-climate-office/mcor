#' Simplified polygons of Montana Counties
#'
#' A dataset containing polygons of Montana counties.
#' These data are [mt_counties], simplified using [rmapshaper::ms_simplify()].
#' These polygons are appropriate for web maps and small print maps.
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
"mt_counties_simple"
