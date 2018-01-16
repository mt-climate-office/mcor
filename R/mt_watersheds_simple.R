#' The Watershed Boundary Dataset Intersecting Montana
#'
#' A dataset containing polygons of Watershed Boundary Dataset (WBD) that intersect Montana.
#' These data are [mt_watersheds], simplified using [rmapshaper::ms_simplify()].
#' These polygons are appropriate for web maps and small print maps.
#' The script that prepared the data is [`data-raw/raw_data.R`]() in the source version of this
#' package.
#'
#' @format A simple feature collection with 56 features and 3 fields:
#' * **WBD code** — the WBD identifier
#' * **Watershed** — the WBD watershed name
#' * **Hydrologic Unit** — the hydrologic unit representing the level of the polygon
#'
#' @source \url{https://nhd.usgs.gov/wbd.html}
"mt_watersheds_simple"
