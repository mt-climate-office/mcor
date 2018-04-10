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

    "https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_future/dataset.html" %>%
    thredds::tds_ncss_list_vars()

}
