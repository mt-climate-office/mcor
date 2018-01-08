library(FedData)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(sf)
library(raster)
library(sp)
library(geosphere)
library(velox)

# The NAD 1983 HARN StatePlane Montana FIPS 2500 coordinate reference system
mt_state_plane <- sf::st_crs(102300)

# Get the Montana county boundary from US Census TIGER database
FedData::download_data("https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip", destdir = "./data-raw")
unzip("./data-raw/tl_2017_us_county.zip", exdir = "./data-raw/tl_2017_us_county")
mt_counties <- sf::st_read("./data-raw/tl_2017_us_county/tl_2017_us_county.shp") %>%
  dplyr::filter(STATEFP == "30") %>%
  sf::st_transform(mt_state_plane) %>%
  dplyr::select(NAME,
                STATEFP,
                COUNTYFP,
                COUNTYNS,
                GEOID) %>%
  dplyr::mutate_at(.vars = vars(NAME:GEOID), .funs = ~as.character(.)) %>%
  dplyr::rename(`County` = NAME,
                `State FIPS code` = STATEFP,
                `County FIPS code` = COUNTYFP,
                `County ANSI code` = COUNTYNS,
                `County GEOID code` = GEOID) %>%
  tibble::as_data_frame() %>%
  sf::st_as_sf()


# Get the official climate division shapefiles
FedData::download_data("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/CONUS_CLIMATE_DIVISIONS.shp.zip", destdir = "./data-raw")
unzip("./data-raw/CONUS_CLIMATE_DIVISIONS.shp.zip", exdir = "./data-raw/CONUS_CLIMATE_DIVISIONS")

mt_climate_divisions <- sf::st_read("./data-raw/CONUS_CLIMATE_DIVISIONS/GIS.OFFICIAL_CLIM_DIVISIONS.shp") %>%
  dplyr::filter(STATE_FIPS == "30") %>%
  dplyr::select(NAME,
                CLIMDIV,
                FIPS_CD) %>%
  tidyr::separate(col = CLIMDIV,
                  into = c("State code","Division code"),
                  sep = 2) %>%
  dplyr::rename(`Division` = NAME,
                `Division FIPS code` = FIPS_CD) %>%
  dplyr::mutate_at(.vars = vars(`Division`:`Division FIPS code`),
                   .funs = ~as.character(.)) %>%
  dplyr::mutate(`Division` = `Division` %>%
                  tolower() %>%
                  tools::toTitleCase()) %>%
  sf::st_transform(mt_state_plane)

mt_counties %<>%
  sf::st_centroid() %>%
  sf::st_intersection(mt_climate_divisions) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::select(`County FIPS code`,
                `Division`,
                `Division code`,
                `Division FIPS code`) %>%
  left_join(mt_counties, .)

mt_climate_divisions <- mt_counties %>%
  dplyr::select(`Division`,
                `Division code`,
                `Division FIPS code`) %>%
  dplyr::group_by(`Division`,
                  `Division code`,
                  `Division FIPS code`) %>%
  summarise() %>%
  sf::st_union(by_feature = TRUE) %>%
  dplyr::arrange(`Division code`) %>%
  dplyr::ungroup()

mt_counties_simple <- mt_counties %>%
  rmapshaper::ms_simplify() %>%
  magrittr::set_names(names(mt_counties))

mt_climate_divisions_simple <- mt_counties_simple %>%
  dplyr::select(`Division`,
                `Division code`,
                `Division FIPS code`) %>%
  dplyr::group_by(`Division`,
                  `Division code`,
                  `Division FIPS code`) %>%
  summarise() %>%
  sf::st_union(by_feature = TRUE) %>%
  dplyr::arrange(`Division code`) %>%
  dplyr::ungroup()

## Generate a hillshade for statewide mapping
aggregate_longlat <- function(x, res, fun = 'mean'){
  scale.x <- geosphere::distGeo(c(xmin(x),mean(ymin(x),ymax(x))),
                                c(xmax(x),mean(ymin(x),ymax(x)))) %>%
    magrittr::divide_by(ncol(x))

  factor.x <- (res/scale.x) %>%
    floor()

  scale.y <- geosphere::distGeo(c(mean(xmin(x),xmax(x)),ymin(x)),
                                c(mean(xmin(x),xmax(x)),ymax(x))) %>%
    magrittr::divide_by(nrow(x))

  factor.y <- (res/scale.y) %>%
    floor()

  x.vx <- velox::velox(x)

  x.vx$aggregate(factor = c(factor.x, factor.y),
                 aggtype = fun)

  if(is(x,"RasterBrick")) return(x.vx$as.RasterBrick())
  if(is(x,"RasterStack")) return(x.vx$as.RasterStack())
  return(x.vx$as.RasterLayer(band=1))
}

mt_rast_500 <- raster::raster(extent(100000,1040000,7000,558000),
                              nrows = 1102,
                              ncols = 1880,
                              crs = CRS("+proj=lcc +lat_1=45 +lat_2=49 +lat_0=44.25 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

mt_ned <- FedData::get_ned(template = mt_counties %>%
                             sf::st_buffer(10000) %>%
                             as("Spatial"),
                           label = "mt",
                           raw.dir = "./data-raw/ned",
                           extraction.dir = "./data-raw/")

system("gdaldem hillshade ./data-raw/mt_NED_1.tif ./data-raw/mt_hillshade.tif -z 2 -s 111120 -multidirectional -co 'COMPRESS=DEFLATE' -co 'ZLEVEL=9'")

mt_hillshade_500m <- raster("./data-raw/mt_hillshade.tif") %>%
  aggregate_longlat(res = 500) %>%
  raster::projectRaster(mt_rast_500) %>%
  raster::crop(mt_counties %>%
                 as("Spatial"),
               snap = 'out') %>%
  raster::mask(mt_counties %>%
                 as("Spatial")) %>%
  round()

mt_hillshade_500m[]=as.integer(mt_hillshade_500m[])

raster::dataType(mt_hillshade_500m) <- "INT1U"

devtools::use_data(mt_state_plane, overwrite = T)
devtools::use_data(mt_counties, overwrite = T)
devtools::use_data(mt_counties_simple, overwrite = T)
devtools::use_data(mt_climate_divisions, overwrite = T)
devtools::use_data(mt_climate_divisions_simple, overwrite = T)
devtools::use_data(mt_hillshade_500m, overwrite = T)

unlink("./data-raw/tl_2017_us_county",
       recursive = TRUE)

unlink("./data-raw/CONUS_CLIMATE_DIVISIONS",
       recursive = TRUE)

unlink("./data-raw/tl_2017_us_county.zip")

unlink("./data-raw/CONUS_CLIMATE_DIVISIONS.shp.zip")

unlink("./data-raw/mt_NED_1.tif")

unlink("./data-raw/mt_hillshade.tif")

unlink("./data-raw/ned",
       recursive = TRUE)
