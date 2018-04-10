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
           ...)
  {

    gridmet <-
      mcor::mco_get_gridmet(dates = c("1981-01-01","2010-12-31"),
                            ...) %>%
      purrr::map(function(x){
        out <- x %>%
          raster::brick()

        raster::projection(out) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

        out.days <- out %>%
          names() %>%
          stringr::str_remove("X") %>%
          lubridate::yday()

        out %<>%
          raster::zApply(by = out.days,
                         fun = mean)

        gc()
        gc()

        out[[1:365]]

      })

    gridmet

  }
