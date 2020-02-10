utils::globalVariables(c('.',
                              'mt_state',
                         'name',
                         'element',
                         'model',
                         'scenario',
                         'var'))
#' Download select MACA V2 datasets.
#'
#' @param x An object of class sf.
#' @param elements A character vector of climate elements.
#' @param models A character vector of datasets
#' @param scenarios A character vector of representative climate pathways
#' @param out_dir A directory in which to download the raw MACA V2 datasets.
#' Defaults to the current working directory
#' @param ... Other parameters passed on to [thredds::tds_ncss_download].
#'
#' @return A raster brick of the desired MACA V2 datasets.
#'
#' @export
#' @importFrom magrittr %>% %<>%
#' @examples
#' \dontrun{
#' mco_get_macav2_monthly(x = mt_state)
#' }
mco_get_macav2_monthly <- function(x = mt_state %>%
                                     sf::st_buffer(10000),
                               elements,
                               models,
                               scenarios,
                               out_dir = tempdir(),
                               ...){

  macav2_datasets <- mco_get_macav2_monthly_datasets() %>%
    dplyr::mutate(var = name) %>%
    tidyr::separate(name,
                    into = c("element",
                             "model",
                             "version",
                             "scenario"),
                    sep = "_")

  if(!missing(elements)){
    macav2_datasets %<>%
      dplyr::filter(element %in% elements)
  }

  if(!missing(models)){
    macav2_datasets %<>%
      dplyr::filter(model %in% models)
  }

  if(!missing(scenarios)){
    macav2_datasets %<>%
      dplyr::filter(scenario %in% scenarios)
  }

  x_bbox <- x %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_transform(4326) %>%
    sf::st_bbox()

  macav2_datasets %>%
    dplyr::mutate(data = var %>%
                    purrr::map(function(var){

                      out_file <- stringr::str_c(out_dir,"/",var,".nc")
                      thredds::tds_ncss_download(ncss_url = "https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_future/dataset.html",
                                                 out_file = out_file,
                                                 bbox = x_bbox,
                                                 vars = var,
                                                 ...) %>%
                        raster::brick()

                    }))

}
