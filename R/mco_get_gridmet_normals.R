utils::globalVariables(c('.',
                         'mt_state'))
#' A wrapper around [mco_get_gridmet] to download GridMET normal dataset (1981--2010).
#' Here, the normals are defined as the daily mean values for the 1981--2010 period.
#'
#' @param x An object of class sf.
#' @param elements Gridmet variables to download.
#' @param ... Additional arguments passed on to [mco_get_gridmet].
#'
#' @return A raster brick of the desired MACA V2 dataset normals.
#'
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' test <- mco_get_gridmet_normals()
#' }
mco_get_gridmet_normals <-
  function(x = mt_state %>%
             sf::st_buffer(10000),
           elements = c("precipitation_amount",
                        "daily_minimum_temperature",
                        "daily_maximum_temperature"),
           out_dir = tempdir(),
           ...)
  {

    if(!file.exists(stringr::str_c(out_dir,"/gridmet_normals.Rds"))){
      gridmet <-
        mcor::mco_get_gridmet(dates = c("1981-01-01","2010-12-31"),
                              elements = elements,
                              out_dir = out_dir,
                              overwrite = FALSE,
                              ...) %>%
        purrr::map(function(x){

          x.days <- x %>%
            names() %>%
            stringr::str_remove("X") %>%
            lubridate::as_date() %>%
            lubridate::yday()

          x %<>%
            raster::setZ(x.days, name = 'yday')

          x %<>%
            raster::zApply(by = x.days,
                           fun = mean)

          gc()
          gc()

          x[[1:365]]

        }) %>%
        readr::write_rds(stringr::str_c(out_dir,"/gridmet_normals.Rds"),
                         compress = "gz")
    }

    readr::read_rds(stringr::str_c(out_dir,"/gridmet_normals.Rds"))

  }
