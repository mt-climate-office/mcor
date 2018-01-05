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
#' * **CLIMATE_DIVISION_ID** — the NOAA Climate Divisional Database (nCLIMDIV) identifier
#' * **CLIMATE_DIVISION_FIPS** — the Federal Information Processing Standard (FIPS) climate division code
#' * **CLIMATE_DIVISION_NAME** — the NOAA Climate Divisional Database (nCLIMDIV) climate division name
#'
#' @source \url{https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ncdc:C00005}
"mt_climate_divisions"
