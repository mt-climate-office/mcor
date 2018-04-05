utils::globalVariables(c("mt_state_simple",
                              "Division code",
                              "Element",
                              "Year",
                              "Month"))

#' Download the NASA Soil Moisture Active Passive (SMAP) data for Montana
#'
#' This is a wrapper for [smapr::find_smap()] and [smapr::download_smap()]
#' that crops the resulting data to a ten kilometer buffer around the
#' state of Montana and allows the user to specify "latest" for the desired
#' `dates`, returning the most recent SMAP product.
#'
#' @param id A character vector of SMAP dataset to extract.\cr
#' \describe{
#' \item{SPL3FTA}{Radar Northern Hemisphere Daily Freeze/Thaw State}
#' \item{SPL3SMA}{Radar Global Daily Soil Moisture}
#' \item{SPL3SMP}{Radiometer Global Soil Moisture}
#' \item{SPL3SMAP}{Radar/Radiometer Global Soil Moisture}
#' \item{SPL4SMAU}{Surface/Rootzone Soil Moisture Analysis Update}
#' \item{SPL4SMGP}{Surface/Rootzone Soil Moisture Geophysical Data}
#' \item{SPL4SMLM}{Surface/Rootzone Soil Moisture Land Model Constants}
#' \item{SPL4CMDL}{Carbon Net Ecosystem Exchange}
#' }
#' @param dates An object of class Date or a character string formatted as
#' %Y-%m-%d (e.g., "2016-04-01") which specifies the date(s) to search.
#' To search for one specific date, this can be a Date object of length one. To
#' search over a time interval, it can be a multi-element object of class Date
#' such as produced by \code{seq.Date}. Use "latest" (the default) to retreive the
#' most recent SMAP product.
#' @param raw_dir A directory in which to download the raw SMAP data. If missing,
#' data are downloaded to a temporary directory.
#'
#' @return A raster brick of the desired SMAP dataset.
#'
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' mco_get_smap()
#' }
mco_get_smap <- function(id = "SPL4SMGP",
                         group = "Geophysical_Data",
                         name = "sm_rootzone",
                         dates = "latest",
                         raw_dir = NULL){

  if(Sys.getenv("ed_un") == ""){
    stop("
This function requires a username and password from NASA's Earthdata portal.
If you have a username and password, pass them in as environment vars using:
Sys.setenv(ed_un = '<your username>', ed_pw = '<your password>')\n
If you do not yet have a username and password, register for one here:
https://urs.earthdata.nasa.gov/")
  }

  if(is.null(raw_dir)){
    raw_dir <- tempdir()
  }

  version <- smapr:::https_prefix() %>%
    smapr:::get_dir_contents() %>%
    stringr::str_subset(id) %>%
    stringr::str_extract('[^.]*$') %>%
    as.integer()

  latest_date <- suppressWarnings(smapr:::route_to_dates(id, version) %>%
                                    smapr:::get_dir_contents() %>%
                                    lubridate::as_date() %>%
                                    max(na.rm = T))

  if(dates == "latest" || dates > latest_date){
    dates <- latest_date
  }

  out <- smapr::find_smap(id = id,
                          dates = dates,
                          version = version) %>%
    smapr::download_smap(directory = raw_dir,
                         overwrite = FALSE) %>%
    smapr:::local_h5_paths()

  out %>%
    purrr::map(function(x){
      x %>%
        rhdf5::h5read(name = stringr::str_c(group,"/",name)) %>%
        smapr:::rasterize_matrix(file = x,
                                 name = group)
    }) %>%
    raster::brick() %>%
    raster::crop(mt_state_simple %>%
                   sf::st_buffer(10000) %>%
                   sf::st_transform("+proj=cea +lat_ts=30 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0") %>%
                   methods::as("Spatial")) %>%
    magrittr::set_names(basename(out))

}
