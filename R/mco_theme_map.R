#' Montana Climate Office theme for maps
#'
#' A **ggplot2** theme that is good for displaying maps from [ggplot2::geom_sf][ggplot2::ggsf].
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @importFrom ggplot2 %+replace% theme_bw element_blank unit element_line
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mt_counties_simple) +
#'   geom_sf(aes(fill = CLIMATE_DIVISION_NAME),
#'               color = "white") +
#'   mco_theme_map()
mco_theme_map <- function(base_size = 9, base_family = ""){
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = 'transparent'),
          panel.grid.minor = ggplot2::element_line(colour = 'transparent'),
          panel.spacing = ggplot2::unit(0,"lines"),
          plot.background = ggplot2::element_blank()
    )
}
