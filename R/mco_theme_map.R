#' Montana Climate Office theme for maps
#'
#' A **ggplot2** theme that is good for displaying maps from [ggplot2::geom_sf][ggplot2::ggsf].
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @importFrom ggplot2 %+replace%
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mt_counties_simple) +
#'   geom_sf(aes(fill = Division),
#'               color = "white") +
#'   mco_theme_map()
#'   }
mco_theme_map <- function(base_size = 6.5,
                          base_family = ""){
  ggplot2::theme_bw(base_size = base_size,
                    base_family = base_family) %+replace%
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = 'transparent'),
                   panel.grid.minor = ggplot2::element_line(colour = 'transparent'),
                   legend.justification = c(0,0),
                   legend.position = c(0,0),
                   legend.background = ggplot2::element_blank(),
                   legend.key.width = ggplot2::unit(0.15,"in"),
                   legend.text = ggplot2::element_text(size = ggplot2::rel(1)),
                   plot.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "npc")
    )
}

