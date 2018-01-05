#' Simplified polygons of Montana Counties
#'
#' A dataset containing polygons of Montana counties.
#' These data are [mt_counties], simplified using [rmapshaper::ms_simplify()].
#' These polygons are appropriate for web maps and small print maps.
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A simple feature collection with 56 features and 8 fields:
#' * **STATE_FIPS** — the Federal Information Processing Standard (FIPS) state code
#' * **COUNTY_FIPS** — the Federal Information Processing Standard (FIPS) county code
#' * **COUNTY_ANSI** — the American National Standards Institute (ANSI) county code
#' * **COUNTY_GEOID** — the US Geographic Identifier (GEOID) county code
#' * **COUNTY_NAME** — the county name
#' * **CLIMATE_DIVISION_ID** — the NOAA Climate Divisional Database (nCLIMDIV) identifier
#' * **CLIMATE_DIVISION_FIPS** — the Federal Information Processing Standard (FIPS) climate division code
#' * **CLIMATE_DIVISION_NAME** — the NOAA Climate Divisional Database (nCLIMDIV) climate division name
#'
#' @source \url{https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip}
"mt_counties_simple"
