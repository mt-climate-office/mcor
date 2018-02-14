#' Polygon of Montana Boundary
#'
#' A dataset containing the Montana boundary polygon.
#' These data are derived from the [mt_counties] dataset;
#' county borders were dissolved using the [sf::st_union] function.
#' This process ensures the extent of the state is the same as the [mt_counties] dataset.
#' These polygons are appropriate for large print maps.
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A geometry set with one feature.
#'
#' @source \url{http://ftp.geoinfo.msl.mt.gov/Data/Spatial/MSDI/AdministrativeBoundaries/MontanaCounties.zip}
"mt_state"
