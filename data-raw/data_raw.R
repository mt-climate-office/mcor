library(FedData)
library(tidyverse)
library(rmapshaper)
library(sf)

# The NAD 1983 HARN StatePlane Montana FIPS 2500 coordinate reference system
mt_state_plane <- sf::st_crs(102300)

# Get the Montana county boundary from US Census TIGER database
FedData::download_data("https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip", destdir = "./data-raw")
unzip("./data-raw/tl_2017_us_county.zip", exdir = "./data-raw/tl_2017_us_county")
mt_counties <- sf::st_read("./data-raw/tl_2017_us_county/tl_2017_us_county.shp") %>%
  dplyr::filter(STATEFP == "30") %>%
  sf::st_transform(mt_state_plane) %>%
  dplyr::select(STATEFP,
                COUNTYFP,
                COUNTYNS,
                GEOID,
                NAME) %>%
  dplyr::mutate_at(.vars = vars(STATEFP:NAME), .funs = ~as.character(.)) %>%
  dplyr::rename(STATE_FIPS = STATEFP,
                COUNTY_FIPS = COUNTYFP,
                COUNTY_ANSI = COUNTYNS,
                COUNTY_GEOID = GEOID,
                COUNTY_NAME = NAME) %>%
  tibble::as_data_frame() %>%
  sf::st_as_sf()


# Get the official climate division shapefiles
FedData::download_data("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/CONUS_CLIMATE_DIVISIONS.shp.zip", destdir = "./data-raw")
unzip("./data-raw/CONUS_CLIMATE_DIVISIONS.shp.zip", exdir = "./data-raw/CONUS_CLIMATE_DIVISIONS")

mt_climate_divisions <- sf::st_read("./data-raw/CONUS_CLIMATE_DIVISIONS/GIS.OFFICIAL_CLIM_DIVISIONS.shp") %>%
  dplyr::filter(STATE_FIPS == "30") %>%
  dplyr::select(CLIMDIV,
                FIPS_CD,
                NAME) %>%
  dplyr::rename(CLIMATE_DIVISION_ID = CLIMDIV,
                CLIMATE_DIVISION_FIPS = FIPS_CD,
                CLIMATE_DIVISION_NAME = NAME) %>%
  dplyr::mutate_at(.vars = vars(CLIMATE_DIVISION_ID:CLIMATE_DIVISION_NAME),
                   .funs = ~as.character(.)) %>%
  dplyr::mutate(CLIMATE_DIVISION_NAME = CLIMATE_DIVISION_NAME %>%
                  tolower() %>%
                  tools::toTitleCase()) %>%
  sf::st_transform(mt_state_plane)

mt_counties %<>%
  sf::st_centroid() %>%
  sf::st_intersection(mt_climate_divisions) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::select(COUNTY_FIPS,
                CLIMATE_DIVISION_ID,
                CLIMATE_DIVISION_FIPS,
                CLIMATE_DIVISION_NAME) %>%
  left_join(mt_counties, .)

mt_climate_divisions <- mt_counties %>%
  dplyr::select(CLIMATE_DIVISION_ID,
                CLIMATE_DIVISION_FIPS,
                CLIMATE_DIVISION_NAME) %>%
  dplyr::group_by(CLIMATE_DIVISION_ID,
                  CLIMATE_DIVISION_FIPS,
                  CLIMATE_DIVISION_NAME) %>%
  summarise() %>%
  sf::st_union(by_feature = TRUE)

mt_counties_simple <- mt_counties %>%
  rmapshaper::ms_simplify()

mt_climate_divisions_simple <- mt_counties_simple %>%
  dplyr::select(CLIMATE_DIVISION_ID,
                CLIMATE_DIVISION_FIPS,
                CLIMATE_DIVISION_NAME) %>%
  dplyr::group_by(CLIMATE_DIVISION_ID,
                  CLIMATE_DIVISION_FIPS,
                  CLIMATE_DIVISION_NAME) %>%
  summarise() %>%
  sf::st_union(by_feature = TRUE)

devtools::use_data(mt_state_plane, overwrite = T)
devtools::use_data(mt_counties, overwrite = T)
devtools::use_data(mt_counties_simple, overwrite = T)
devtools::use_data(mt_climate_divisions, overwrite = T)
devtools::use_data(mt_climate_divisions_simple, overwrite = T)

unlink("./data-raw/tl_2017_us_county",
       recursive = TRUE)

unlink("./data-raw/CONUS_CLIMATE_DIVISIONS",
       recursive = TRUE)

unlink("./data-raw/tl_2017_us_county.zip")

unlink("./data-raw/CONUS_CLIMATE_DIVISIONS.shp.zip")
