#' Polygons of Montana Climate Divisions
#'
#' A dataset containing polygons of Montana climate divisions.
#' These data are derived from the [mt_counties] dataset;
#' county borders were dissolved using the [sf::summarize.sf()][sf::dplyr] function.
#' This process ensures the extent of the state is the same as the [mt_counties] dataset.
#' These polygons are appropriate for large print maps.
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A simple feature collection with 56 features and 3 fields:
#' * **Division** — the NOAA Climate Divisional Database (nCLIMDIV) climate division name
#' * **Division code** — the NOAA Climate Divisional Database (nCLIMDIV) identifier
#' * **Division FIPS code** — the Federal Information Processing Standard (FIPS) climate division code
#'
#' @source \url{https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ncdc:C00005}
"mt_climate_divisions"
