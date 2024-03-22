#' Basic leaflet plotting using `mcor` style
#'
#'
#' @export
#' @importFrom magrittr %>%
mco_leaflet <- function(x,
                        pal =  "RdBu",
                        legend_title = "",
                        image_query_title = "",
                        midpoint = 0,
                        digits = 1,
                        reverse = FALSE,
                        pane = "foreground",
                        attribution = "Climate data by <a href='http://www.climatologylab.org/gridmet.html' target='_blank'>gridMET</a> via <a href='https://earthengine.google.com/' target='_blank'>Google Earth Engine</a>",
                        ...){

  tm_out <- (x %>%
               tmap::tm_shape() +
               tmap::tm_raster(title = "",
                               # legend.show = FALSE,
                               alpha = 1,
                               style= "cont",
                               # n = 10,
                               palette = pal,
                               midpoint = midpoint,
                               legend.reverse = reverse,
                               legend.is.portrait = TRUE) +
               # tmap::tm_layout(title = legend_title) +
               tmap::tm_view(view.legend.position = c("left","bottom"))) %>%
    tmap::tmap_leaflet()

  tm_out$x$calls[[5]]$args[[5]] <- image_query_title

  out <- mcor::mco_leaflet_base(attribution = attribution) %>%
    leaflet::addRasterImage(x,
                            group = image_query_title) %>%
    leafem::addImageQuery(x = x,
                          # type = "click",
                          group = image_query_title,
                          layerId = image_query_title,
                          prefix = "",
                          digits = digits,
                          position = "bottomleft",
    )

  if(reverse){
    tm_out$x$calls[[6]]$args[[1]]$labels %<>% rev()
  }


  # out$x$calls[[11]]$args[[4]]$pane <- pane
  # tm_out$x$calls[[4]]$args[[4]]$pane <- "background"
  # tm_out$x$calls[[4]]$args[[4]]$attribution <- ""
  # tm_out$x$calls[[4]]$args[[6]] <- ""
  # out$x$calls[[length(out$x$calls)]]$args[[4]]$pane <- "foreground"



  out$x$calls <- c(out$x$calls,tm_out$x$calls[5:6])

  out$title <- tm_out$title

  # tm_out$jsHooks$render[[1]]$code %<>%
  #   stringr::str_replace("document.getElementsByClassName","el.getElementsByClassName")

  out$jsHooks$render <- c(out$jsHooks$render,
                          list(list(code =
                                      paste0("function(el, x) {var tldiv = document.getElementsByClassName('leaflet-top leaflet-left')[0];var titlediv = document.createElement('div');titlediv.className = 'info legend leaflet-control';titlediv.innerHTML = \"<b>",
                                             legend_title,
                                             "</b>\";tldiv.insertBefore(titlediv, tldiv.childNodes[0]);}"),
                                    data = NULL)))

  out$jsHooks$render %<>%
    purrr::map(function(x){
      x$code %<>%
        stringr::str_remove_all("\\t") %>%
        stringr::str_remove_all("\\n")

      return(x)
    })

  # stars <- out$dependencies %>%
  #   purrr::keep(~ .x$name == "stars") %>%
  #   magrittr::extract2(1) %$%
  #   paste0(src$file,"/",script[[1]]) %>%
  #   readr::read_file() %>%
  #   htmltools::tags$script()
  #
  # out$dependencies %<>%
  #   purrr::discard(~ .x$name == "stars")
  #
  # # out$jsHooks$render <- c(out$jsHooks$render,
  # #                         list(list(code = stars,
  # #                              data = NULL)))
  #
  # out %<>%
  #   htmlwidgets::appendContent(stars) #%>%
  #   # leaflet.opacity::addOpacitySlider(layerId = image_query_title)

  out

}


#' Basic leaflet plotting using `mcor` style
#'
#'
#' @export
#' @importFrom magrittr %>%
mco_leaflet_base <- function(attribution = ""){

leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
    leaflet::addMapPane("background", zIndex = 1) %>%
    leaflet::addMapPane("middleground", zIndex = 420) %>%
    leaflet::addMapPane("foreground", zIndex = 430) %>%
    leaflet::addTiles(httr::modify_url("https://tiles.stadiamaps.com/tiles/stamen_toner_background/{z}/{x}/{y}@2x.png",
                                       query = list(key = keyring::key_get("stadia"))),
                      # attribution = attribution,
                      options = leaflet::tileOptions(pane = "background")) %>%
    leaflet::addTiles(httr::modify_url("https://api.maptiler.com/tiles/hillshade/{z}/{x}/{y}.webp",
                                       query = list(key = keyring::key_get("maptiler"))),
                      # attribution = attribution,
                      options = leaflet::tileOptions(pane = "foreground")) %>%
    leaflet::addTiles(httr::modify_url("https://tiles.stadiamaps.com/tiles/stamen_toner_lines/{z}/{x}/{y}@2x.png",
                                       query = list(key = keyring::key_get("stadia"))),
                      # attribution = attribution,
                      options = leaflet::tileOptions(pane = "foreground")) %>%
    leaflet::addTiles(httr::modify_url("https://tiles.stadiamaps.com/tiles/stamen_toner_labels/{z}/{x}/{y}@2x.png",
                                       query = list(key = keyring::key_get("stadia"))),
                      # attribution = attribution,
                      options = leaflet::tileOptions(pane = "foreground")) %>%
    leaflet::addScaleBar(position = "bottomright",
                         options = leaflet::scaleBarOptions(metric = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topleft' }).addTo(this)}") %>%
    leaflet.extras::addFullscreenControl(position = "topleft")

}

