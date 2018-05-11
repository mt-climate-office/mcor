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
           p = c(0,
                 0.025,
                 0.25,
                 0.5,
                 0.75,
                 0.975,
                 1),
           overwrite = FALSE,
           ...)
  {

    if(overwrite)
      unlink(stringr::str_c(out_dir,"/gridmet_normals.Rds"))

    if(!file.exists(stringr::str_c(out_dir,"/gridmet_normals.Rds"))){
      p.names <- stringr::str_c(p*100, "%")

      gridmet <-
        mcor::mco_get_gridmet(dates = c("1981-01-01","2010-12-31"),
                              elements = elements,
                              out_dir = out_dir,
                              overwrite = FALSE,
                              ...) %>%
        purrr::map(function(norm){

          the.days <- norm %>%
            names() %>%
            stringr::str_remove("X") %>%
            lubridate::as_date() %>%
            lubridate::yday()

          purrr::map(1:365,
                     function(the.day){
                       vals <- norm %>%
                         magrittr::extract2(which(the.days == the.day)) %>%
                         raster::values()

                       raster::setValues(norm,
                                         p %>%
                                           magrittr::set_names(p.names) %>%
                                           purrr::map(~biwavelet::rcpp_row_quantile(vals,
                                                                                    q = .x)) %>%
                                           do.call(cbind, .) %>%
                                           tibble::as_tibble() %>%
                                           dplyr::mutate(mean = vals %>%
                                                           matrixStats::rowMeans2()) %>%
                                           as.matrix()
                       ) %>%
                         raster::as.list() %>%
                         magrittr::set_names(c(p.names,
                                               "mean"))
                     }) %>%
            purrr::transpose() %>%
            purrr::map(raster::stack,
                       quick = TRUE)
        }) %>%
        # fst::write_fst(stringr::str_c(out_dir,"/gridmet_normals.fst"))
        readr::write_rds(stringr::str_c(out_dir,"/gridmet_normals.Rds"),
                         compress = "bz")
    }
    # fst::read_fst(stringr::str_c(out_dir,"/gridmet_normals.fst"))
    readr::read_rds(stringr::str_c(out_dir,"/gridmet_normals.Rds"))

  }
