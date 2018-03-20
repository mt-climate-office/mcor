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
                         "Watershed"))

#' Download and process the Montana SNOTEL Snow Water Equivalent dataset from the
#' NRCS National Water and Climate Center
#'
#' @param date A character string of either the date (in yyyy-mm-dd format) or
#' "latest" to get the latest records. Defaults to "latest".
#' @param huc An integer indicating the Watershed Boundary Hydrological Unit level,
#' either "6" or "8", over which to aggregate station data. Defaults to 6.
#' @param min_stations An integer indicating the minimum number of stations that
#' must be available in a region in order to calculate a regional average SWE.
#' Defaults to 3.
#'
#' @return A simple feature collection with 9 fields:
#' * **WBD code** — the WBD identifier
#' * **Watershed** — the WBD watershed name
#' * **Stations** — the count of stations aggregated to generate a value for the watershed
#' * **SWE (in)** — the mean of Snow Water Equivalent (in) start of day values of stations in the watershed
#' * **SWE 1981-2010 Median (in)** — the mean of normal (1981-2010) median Snow Water Equivalent (in) start of day values of stations in the watershed
#' * **Percent SWE** — `SWE (in)` / `SWE 1981-2010 Median (in)`
#'
#' @export
#' @importFrom magrittr %>% %$%
#' @examples
#' \dontrun{
#' mco_get_swe_basins()
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
