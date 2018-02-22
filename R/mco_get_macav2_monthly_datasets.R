utils::globalVariables(c('.',
                              'html'))
#' Get a list of the available MACA V2 datasets.
#'
#' @return A character vector of dataset names.
#'
#' @export
#' @importFrom magrittr %>%
#' @examples
#' mco_get_macav2_monthly_datasets()
mco_get_macav2_monthly_datasets <- function(){
  "http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html" %>%
    # "https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_future/dataset.html" %>%
    xml2::read_html() %>%
    xml2::as_list() %$%
    html %$%
    body %$%
    table %>%
    purrr::map(function(x){x$td$a %>% attr("href")}) %>%
    unlist() %>%
    magrittr::set_names(NULL) %>%
    gsub("reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html?dataset=",
         "",
         x = .,
         fixed = T)
}
