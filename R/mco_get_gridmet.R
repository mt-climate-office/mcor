utils::globalVariables(c('.',
                         'mt_state'))
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
#' @param start_date An optional date to download.
#' @param date An optional date to download.
#' @param raw_dir A directory in which to download the raw MACA V2 datasets.
#' Defaults to the current working directory
#'
#' @return A raster brick of the desired MACA V2 datasets.
#'
#' @export
#' @importFrom magrittr %>%
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
                            out_dir){

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
      ncss_args = list(accept = "netcdf4",
                       time = dates)
    }
  }

  out <- thredds::tds_list_datasets("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html") %>%
    dplyr::mutate(Service = path %>%
                    purrr::map_chr(function(x){
                      out <- thredds::tds_list_services(x)
                      out$path[out$service == "NetcdfSubset"]
                    }),
                  Variable = Service %>%
                    purrr::map_chr(function(x){
                      thredds::tds_ncss_list_vars(x)$name[[1]]
                    })) %>%
    dplyr::filter(Variable %in% elements) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Filename = thredds::tds_ncss_download(ncss_url = Service,
                                                       bbox = x %>%
                                                         lwgeom::st_transform_proj(4326) %>%
                                                         sf::st_bbox(),
                                                       vars = Variable,
                                                       out_file = stringr::str_c(out_dir,"/",Variable,".nc"),
                                                       ncss_args = ncss_args),
                  Data = Filename %>%
                    raster::stack() %>%
                    raster::t() %>%
                    raster::flip("x") %>%
                    list())

  purrr::map2(out$Data, out$Filename, function(x,y){
      names(x) <- y %>%
        raster::stack() %>%
        names() %>%
        stringr::str_remove("X") %>%
        as.integer() %>%
        magrittr::add(lubridate::as_date("1900-01-01"))

      projection(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

      x
    }) %>%
    magrittr::set_names(out$Variable)

}
