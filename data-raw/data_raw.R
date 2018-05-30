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

# Get the Montana county boundary dataset from the Montana Spatial Data Infrastructure
FedData::download_data("http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaCounties.zip", destdir = "./data-raw")
unzip("./data-raw/MontanaCounties.zip", exdir = "./data-raw/MontanaCounties")
mt_counties <- sf::st_read("./data-raw/MontanaCounties/MontanaCounties.gdb", layer = "County") %>%
  lwgeom::st_transform_proj(mt_state_plane) %>%
dplyr::mutate(`State FIPS code` = "30") %>%
dplyr::select(NAMELABEL,
              `State FIPS code`,
              FIPS) %>%
dplyr::mutate_at(.vars = vars(NAMELABEL:FIPS), .funs = ~as.character(.)) %>%
dplyr::rename(`County` = NAMELABEL,
              `County FIPS code` = FIPS) %>%
tibble::as_data_frame() %>%
sf::st_as_sf()

# Get the Montana county boundary from US Census TIGER database
# FedData::download_data("https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip", destdir = "./data-raw")
# unzip("./data-raw/tl_2017_us_county.zip", exdir = "./data-raw/tl_2017_us_county")
# mt_counties <- sf::st_read("./data-raw/tl_2017_us_county/tl_2017_us_county.shp") %>%
#   dplyr::filter(STATEFP == "30") %>%
#   lwgeom::st_transform_proj(mt_state_plane) %>%
#   dplyr::select(NAME,
#                 STATEFP,
#                 COUNTYFP,
#                 COUNTYNS,
#                 GEOID) %>%
#   dplyr::mutate_at(.vars = vars(NAME:GEOID), .funs = ~as.character(.)) %>%
#   dplyr::rename(`County` = NAME,
#                 `State FIPS code` = STATEFP,
#                 `County FIPS code` = COUNTYFP,
#                 `County ANSI code` = COUNTYNS,
#                 `County GEOID code` = GEOID) %>%
#   tibble::as_data_frame() %>%
#   sf::st_as_sf()

# Get the Montana tribal land boundary dataset from the Montana Spatial Data Infrastructure
FedData::download_data("http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaReservations.zip", destdir = "./data-raw")
unzip("./data-raw/MontanaReservations.zip", exdir = "./data-raw/MontanaReservations")
mt_tribal_land <- sf::st_read("./data-raw/MontanaReservations/MontanaReservations.gdb", layer = "MontanaReservations") %>%
  lwgeom::st_transform_proj(mt_state_plane) %>%
  dplyr::select(NAME) %>%
  dplyr::mutate(NAME = NAME %>%
                  as.character() %>%
                  tolower() %>%
                  tools::toTitleCase()) %>%
  dplyr::rename(`Name` = NAME) %>%
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
  lwgeom::st_transform_proj(mt_state_plane)

mt_counties %<>%
  sf::st_centroid() %>%
  sf::st_intersection(mt_climate_divisions) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::select(`County FIPS code`,
                `Division`,
                `Division code`,
                `Division FIPS code`) %>%
  left_join(mt_counties, .) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf()

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
  dplyr::ungroup() %>%
  tibble::as_tibble() %>%
  sf::st_as_sf()

mt_counties_simple <- mt_counties %>%
  rmapshaper::ms_simplify() %>%
  magrittr::set_names(names(mt_counties)) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf()

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
  dplyr::ungroup() %>%
  tibble::as_tibble() %>%
  sf::st_as_sf()

mt_state <- mt_counties %>%
  sf::st_union() %>%
  tibble::as_tibble() %>%
  sf::st_as_sf()

mt_state_simple <- mt_counties_simple %>%
  sf::st_union() %>%
  tibble::as_tibble() %>%
  sf::st_as_sf()

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

## Get the Watershed Boundary Dataset for Montana
FedData::download_data("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip",
                       destdir = "./data-raw")
unzip("./data-raw/WBD_National_GDB.zip",
      exdir = "./data-raw/WBD_National_GDB")

# The latest HUC12 is corrupt (Jan 3, 2018), so this goes through HUC10, and then we supplement.
mt_watersheds <- seq(2,10,2) %>%
  purrr::map(function(hu){
    layer_name <- paste0("WBDHU",hu)

    out <- sf::st_read("./data-raw/WBD_National_GDB/WBD/WBD.gdb",
                       layer = layer_name) %>%
      dplyr::filter(grepl("MT",STATES)) %>%
      dplyr::select(dplyr::starts_with("HUC"),
                    NAME) %>%
      dplyr::mutate(`Hydrologic Unit` = factor(hu,
                                               levels = seq(2,12,2),
                                               ordered = TRUE)) %>%
      dplyr::rename(Watershed = NAME,
                    geometry = SHAPE)

    names(out)[1] <- "WBD code"

    return(out)
  }) %>%
  do.call(rbind,.) %>%
  lwgeom::st_transform_proj(mt_state_plane) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf()

# # Download HUC12 dataset from https://nrcs.app.box.com/v/gateway/folder/39640323180
# unzip("./data-raw/wbdhu12_a_us_september2017.zip",
#       exdir = "./data-raw/wbdhu12_a_us_september2017")
#
# huc12 <- sf::st_read("./data-raw/wbdhu12_a_us_september2017/wbdhu12_a_us_september2017.gdb",
#                    layer = "WBDHU12") %>%
#   dplyr::filter(grepl("MT",STATES)) %>%
#   dplyr::select(dplyr::starts_with("HUC"),
#                 NAME) %>%
#   dplyr::mutate(`Hydrologic Unit` = factor(12,
#                                            levels = seq(2,12,2),
#                                            ordered = TRUE)) %>%
#   dplyr::rename(Watershed = NAME,
#                 geometry = Shape) %>%
#   lwgeom::st_transform_proj(mt_state_plane) %>%
#   tibble::as_tibble() %>%
#   sf::st_as_sf()
#
# names(huc12)[1] <- "WBD code"
#
# mt_watersheds %<>%
#   rbind(huc12)

mt_watersheds_simple <- mt_watersheds %>%
  rmapshaper::ms_simplify() %>%
  magrittr::set_names(names(mt_watersheds))

devtools::use_data(mt_state_plane, overwrite = T)
devtools::use_data(mt_state, overwrite = T)
devtools::use_data(mt_state_simple, overwrite = T)
devtools::use_data(mt_counties, overwrite = T)
devtools::use_data(mt_counties_simple, overwrite = T)
devtools::use_data(mt_tribal_land, overwrite = T)
devtools::use_data(mt_climate_divisions, overwrite = T)
devtools::use_data(mt_climate_divisions_simple, overwrite = T)
devtools::use_data(mt_hillshade_500m, overwrite = T)
devtools::use_data(mt_watersheds, overwrite = T)
devtools::use_data(mt_watersheds_simple, overwrite = T)

unlink("./data-raw/tl_2017_us_county",
       recursive = TRUE)

unlink("./data-raw/MontanaCounties.zip")

unlink("./data-raw/MontanaCounties/",
       recursive = TRUE)

unlink("./data-raw/MontanaReservations.zip")

unlink("./data-raw/MontanaReservations/",
       recursive = TRUE)

unlink("./data-raw/CONUS_CLIMATE_DIVISIONS",
       recursive = TRUE)

unlink("./data-raw/tl_2017_us_county.zip")

unlink("./data-raw/CONUS_CLIMATE_DIVISIONS.shp.zip")

unlink("./data-raw/mt_NED_1.tif")

unlink("./data-raw/mt_hillshade.tif")

unlink("./data-raw/ned",
       recursive = TRUE)

unlink("./data-raw/wbdhu12_a_us_september2017.zip")

unlink("./data-raw/wbdhu12_a_us_september2017",
       recursive = TRUE)

unlink("./data-raw/WBD_National_GDB.zip")

unlink("./data-raw/WBD_National_GDB",
       recursive = TRUE)


