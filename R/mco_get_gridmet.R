utils::globalVariables(c('.',
                         'mt_state',
                         'path',
                         'service',
                         'Variable',
                         'Filename'))
#' Download select GridMET datasets.
#'
#' @param x An object of class sf.
#' @param elements Gridmet variables to download.
#' @param dates An object of class Date or a character string formatted as
#' %Y-%m-%d (e.g., "2016-04-01") which specifies the date(s) to search.
#' To search for one specific date, this can be a Date object of length one. To
#' return data over a time interval, it can be a vector of length 2
#' (e.g., c("2018-01-01","2018-03-31")). Use "latest" (the default) to retreive the
#' most recent date available product.
#' @param out_dir A directory in which to download the raw MACA V2 datasets.
#' Defaults to the current working directory
#' @param overwrite Whether to overwrite a file in the out_dir of the same element.
#' @param ... Other parameters passed on to thredds::tds_ncss_download.
#'
#' @return A raster brick of the desired MACA V2 datasets.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom raster projection<-
#' @examples
#' \dontrun{
#' test <- mco_get_gridmet()
#' test <- mco_get_gridmet(dates = "2018-02-14")
#' test <- mco_get_gridmet(dates = "2018-03-31")
#' test <- mco_get_gridmet(dates = c("2018-01-01","2018-04-01"))
#' }
mco_get_gridmet <- function(x = mt_state %>%
                              sf::st_buffer(10000),
                            elements = c("precipitation_amount",
                                         "daily_minimum_temperature",
                                         "daily_maximum_temperature"),
                            dates = "latest",
                            out_dir = tempdir(),
                            overwrite = TRUE,
                            ...){

  # # Fix for flipped GridMet grid
  # x <- gridmet_fix %>%
  #   raster::flip("y") %>%
  #   mcor::mco_mask(x) %>%
  #   raster::flip("y") %>%
  #   raster::trim() %>%
  #   FedData::polygon_from_extent() %>%
  #   sf::st_as_sf()
  #
  # x_real <- mcor:::gridmet_fix %>%
  #   # raster::flip("y") %>%
  #   mcor::mco_mask(x) %>%
  #   raster::flip("y") %>%
  #   raster::trim() %>%
  #   FedData::polygon_from_extent() %>%
  #   sf::st_as_sf()

  if(missing(out_dir)){
    out_dir <- tempfile()
    dir.create(out_dir,
               recursive = TRUE,
               showWarnings = FALSE)
  }

  if(length(dates) > 2)
    stop("Dates must be a vector of length 2 or less.")

  if (length(dates) == 2) {
    if(dates[1] < dates[2]){
      ncss_args = list(accept = "netcdf4",
                       time_start = dates[1],
                       time_end = dates[2])
    }else{
      ncss_args = list(accept = "netcdf4",
                       time_start = dates[2],
                       time_end = dates[1])
    }

  } else {
    if(dates == "latest" || dates > Sys.Date()){
      ncss_args = list(accept = "netcdf4")
    } else {
      ncss_args = list(time = dates,
                       accept = "netcdf4")
    }
  }

  services <- "http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html" %>%
    thredds::tds_list_datasets() %$%
    path %>%
    purrr::map_dfr(function(path){
      path %>%
        thredds::tds_list_services() %>%
        dplyr::filter(service %in% "NetcdfSubset")
    })

  vars <- services %$%
    path %>%
    purrr::map_dfr(function(path){
      path %>%
        thredds::tds_ncss_list_vars()
    }) %>%
    dplyr::bind_cols(services) %>%
    dplyr::filter(name %in% elements)

  files <- purrr::map2(vars$name,
                       vars$path,
                       function(name,path){
                         thredds::tds_ncss_download(ncss_url = path,
                                                    bbox =
                                                      x %>%
                                                      sf::st_transform(4326) %>%
                                                      sf::st_bbox(),
                                                    vars = name,
                                                    out_file = stringr::str_c(out_dir,"/",name,".nc"),
                                                    ncss_args = ncss_args,
                                                    overwrite = overwrite,
                                                    ...)
                       }) %>%
    magrittr::set_names(vars$name)

  # Fix malformed NetCDF files, and write to disk
  purrr::imap(files,
              function(file,name){
                if(!file.exists(stringr::str_c(out_dir,"/",name,".Rds")) | overwrite){
                  out <-
                    file %>%
                    raster::stack() %>%
                    raster::readAll()# %>%
                    # raster::flip("y")

                  # out %<>%
                  #   raster::setExtent(out %>%
                  #                       raster::extend(gridmet_fix) %>%
                  #                       raster::flip("y") %>%
                  #                       raster::trim() %>%
                  #                       extent())

                  names(out) <- file %>%
                    raster::stack() %>%
                    names() %>%
                    stringr::str_remove("X") %>%
                    as.integer() %>%
                    magrittr::add(lubridate::as_date("1900-01-01"))

                  raster::projection(out) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

                  out %>%
                    readr::write_rds(stringr::str_c(out_dir,"/",name,".Rds"),
                                     compress = "bz")
                }

                readr::read_rds(stringr::str_c(out_dir,"/",name,".Rds"))

              }) %>%
    magrittr::set_names(vars$name)

}
