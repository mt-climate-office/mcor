utils::globalVariables(c("mt_state_plane",
                         "Start Date",
                         "Station Id",
                         "State Code",
                         "Network Code",
                         "station",
                         "WBD code",
                         "mt_watersheds_simple",
                         "Hydrologic Unit",
                         "Snow Water Equivalent (in) Start of Day Values",
                         "Median Snow Water Equivalent (1981-2010) (in) Start of Day Values",
                         "n",
                         "Stations Count",
                         "SWE (in)",
                         "SWE 1981-2010 Median (in)",
                         "mt_counties_simple",
                         "m",
                         "Watershed",
                         'Station',
                         'Priority'))

#' Download and process the Montana SNOTEL Snow Water Equivalent dataset from the
#' NRCS National Water and Climate Center
#'
#' @return A data_frame with 2 fields:
#' * **Station** — the station identifier in trinomial format.
#' * **WBD code** — the WBD watershed code
#'
#' @export
#' @importFrom magrittr %>% %$%
#' @examples
#' \dontrun{
#' mco_get_snotel_huc()
#' }
mco_get_snotel_huc <- function(){

  suppressWarnings("https://www.wcc.nrcs.usda.gov/ftpref/data/water/wcs/gis/data/getdata/hucassociations.csv" %>%
    readr::read_csv(col_names = c("Station", 1:10))) %>%
    tidyr::gather(key = "Priority",
                  value = "WBD code",
                  -Station) %>%
    # dplyr::mutate(`WBD code` = stringr::str_sub(`WBD code`,1,huc)) %>%
    # dplyr::filter(`WBD code` %in% (mt_watersheds_simple %>%
    #                                  dplyr::filter(`Hydrologic Unit` %in% c(huc)) %$%
    #                                  `WBD code`)) %>%
    stats::na.omit() %>%
    dplyr::distinct() %>%
    dplyr::select(-Priority)

}
