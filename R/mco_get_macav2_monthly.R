utils::globalVariables(c('.',
                              'mt_state'))
#' Download select MACA V2 datasets.
#'
#' @param x An object of class sf.
#' @param elements A character vector of climate elements.
#' @param models A character vector of datasets
#' @param scenarios A character vector of representative climate pathways
#' @param raw_dir A directory in which to download the raw MACA V2 datasets.
#' Defaults to the current working directory
#'
#' @return A raster brick of the desired MACA V2 datasets.
#'
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' mco_get_macav2_monthly(x = mt_state)
#' }
mco_get_macav2_monthly <- function(x = NULL,
                               elements = NULL,
                               models = NULL,
                               scenarios = NULL,
                               raw_dir = "./"){

  macav2_datasets <- mco_get_macav2_monthly_datasets()

  if(!is.null(elements)){
    macav2_datasets <- elements %>%
      stringr::str_c("_",.,"_") %>%
      purrr::map(~stringr::str_subset(macav2_datasets, .x)) %>%
      unlist() %>%
      sort()
  }

  if(!is.null(models)){
    macav2_datasets <- models %>%
      stringr::str_c("_",.,"_") %>%
      purrr::map(~stringr::str_subset(macav2_datasets, .x)) %>%
      unlist() %>%
      sort()
  }

  if(!is.null(scenarios)){
    macav2_datasets <- scenarios %>%
      stringr::str_c("_",.,"_") %>%
      purrr::map(~stringr::str_subset(macav2_datasets, .x)) %>%
      unlist() %>%
      sort()
  }

  if(is.null(x))
    x <- mt_state %>%
      sf::st_buffer(10000)

  x_bbox <- x %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    lwgeom::st_transform_proj(4326) %>%
    # magrittr::add(c(360,0)) %>%
    sf::st_bbox()

  macav2_datasets %>%
    purrr::map(function(dataset){
      tryCatch({


        out_file <- stringr::str_c(raw_dir,"/",dataset,".nc")

        if(!file.exists(out_file)){
          nc <- ncdf4::nc_open(stringr::str_c('http://thredds.northwestknowledge.net:8080/thredds/dodsC/',dataset,'.nc'))
          var <- names(nc$var)
          ncdf4::nc_close(nc)

          httr::GET(stringr::str_c("http://thredds.northwestknowledge.net:8080/thredds/ncss/",dataset,".nc",
                                   "?",
                                   "var=", var,
                                   "&north=", x_bbox[["ymax"]],
                                   "&west=", x_bbox[["xmin"]],
                                   "&east=", x_bbox[["xmax"]],
                                   "&south=", x_bbox[["ymin"]],
                                   "&disableProjSubset=on",
                                   "&horizStride=1",
                                   "&temporal=all",
                                   "&addLatLon=true",
                                   "&accept=netcdf4"),
                    httr::write_disk(out_file, overwrite = TRUE))
        }

        raster::brick(out_file)

      },
      error = function(e){return(NULL)})
    })
}
