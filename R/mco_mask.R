#' Mask a raster using fasterize
#'
#' @param rast A Raster* object to be masked
#' @param mask A simple features polygon dataset to act as the mask.
#'
#' @return A masked Raster* object.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' library(mcor)
#' library(magrittr)
#'
#' raster::plot(mt_hillshade_500m)
#'
#' # Mask to Missoula county
#' mco_mask(rast = mt_hillshade_500m,
#'          mask = dplyr::filter(mt_counties, County == "Missoula")) %>%
#'    raster::plot()
#' }
mco_mask <- function(rast, mask){
  raster::mask(rast,
               fasterize::fasterize(mask %>%
                                      sf::st_transform(raster::projection(rast)),
                                    raster::raster(rast))
  )
}
