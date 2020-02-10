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
                         'End Date',
                         'Station'))

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
#' * **SWE 1981-2010 Median (in)** — the mean of normal (1981-2010) average Snow Water Equivalent (in) start of day values of stations in the watershed
#' * **Percent SWE** — `SWE (in)` / `SWE 1981-2010 Median (in)`
#'
#' @export
#' @importFrom magrittr %>% %$%
#' @examples
#' \dontrun{
#' mco_get_swe_basins()
#' }
mco_get_swe_basins <- function(date = "latest",
                               huc = 6,
                               min_stations = 3){

  if(is.character(date) && date == "latest")
    date <- Sys.Date()

  snotel_inventory <- mco_get_snotel_inventory() %>%
    dplyr::left_join(mco_get_snotel_huc() %>%
                       dplyr::mutate(`WBD code` = stringr::str_sub(`WBD code`,1,huc)) %>%
                       dplyr::distinct()) %>%
    dplyr::filter(`WBD code` %in% (mt_watersheds_simple %>%
                                     dplyr::filter(`Hydrologic Unit` %in% c(huc)) %$%
                                     `WBD code`)) %>%
    sf::st_as_sf() %>%
    sf::st_transform(mt_state_plane) %>%
    dplyr::select(-`Station Id`:-`End Date`)

  snotel_data <- mco_get_snotel_data(stations = snotel_inventory$Station %>%
                                       unique(),
                                     variables = c('WTEQ::value',
                                                   'WTEQ::median_1981'),
                                     start_date = date,
                                     end_date = date)

  snotel_inventory %>%
    dplyr::left_join(snotel_data,
                     by = c("Station")) %>%
    dplyr::arrange(Station) %>%
    stats::na.omit() %>%
    dplyr::select(`WBD code`,
                  `Snow Water Equivalent (in) Start of Day Values`,
                  `Median Snow Water Equivalent (1981-2010) (in) Start of Day Values`) %>%
    dplyr::group_by(`WBD code`) %>%
    dplyr::summarise(`Stations Count` = n(),
                     `SWE (in)` = mean(`Snow Water Equivalent (in) Start of Day Values`),
                     `SWE 1981-2010 Median (in)` = mean(`Median Snow Water Equivalent (1981-2010) (in) Start of Day Values`)) %>%
    dplyr::filter(`Stations Count` >= min_stations) %>%
    dplyr::mutate(`Percent SWE` = round(100 * `SWE (in)`/`SWE 1981-2010 Median (in)`) %>%
                    as.integer()) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::left_join(mt_watersheds_simple) %>%
    sf::st_as_sf() %>%
    dplyr::select(-`Hydrologic Unit`) %>%
    dplyr::select(`WBD code`, Watershed, dplyr::everything())

}
