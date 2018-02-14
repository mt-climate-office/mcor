#' Simplified Polygon of Montana Boundary
#'
#' A dataset containing the Montana boundary polygon.
#' These data are [mt_counties], simplified using [rmapshaper::ms_simplify()]
#' and then dissolved using the [sf::st_union] function.
#' These polygons are appropriate for web maps and small print maps.
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A geometry set with one feature.
#'
#' @source \url{http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaCounties.zip}
"mt_state_simple"
