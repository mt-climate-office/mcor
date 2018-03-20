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

#' Download and process the Montana SNOTEL site inventory from the
#' NRCS National Water and Climate Center
#'
#' @return A simple feature collection with 6 fields:
#' * **Station Id** — the SNOTEL station identification number
#' * **Station Name** — the SNOTEL station location name
#' * **State Code** — the two letter state code
#' * **`Network Code`** — the code for the station network
#' * **Start Date** — the date the station started recording data
#' * **End Date** — the date the station stopped recording data.
#' Will be "2100-01-01" if station is still recording.
#'
#' @export
#' @importFrom magrittr %>% %$%
#' @examples
#' \dontrun{
#' mco_get_snotel_inventory()
#' }
mco_get_snotel_data <- function(stations = c("1040:CO:SNTL",
                                             "619:OR:SNTL",
                                             "1048:NM:SNTL"),
                                variables = c('stationId',
                                              'WTEQ::value'),
                                start_date = "POR_BEGIN",
                                end_date = "POR_END"){

  suppressWarnings(readr::read_csv(paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/start_of_period/",
                                          paste0(stations %>% unique(),
                                                 collapse = "|"),
                                          '|name/',
                                          start_date,',',end_date,'/',
                                          stringr::str_c(c(variables,
                                                           'stationId',
                                                           'state.code',
                                                           'network.code') %>%
                                                           unique(),
                                                         collapse = ',')),
                                   comment = "#",
                                   col_types = readr::cols(
                                     `Date` = readr::col_date(format = ""),
                                     `Station Id` = readr::col_integer(),
                                     `Snow Water Equivalent (in) Start of Day Values` = readr::col_double()
                                   )
  )) %>%
    dplyr::mutate(Station = paste0(`Station Id`,":",
                                   `State Code`,":",
                                   `Network Code`)) %>%
    dplyr::select(-`Station Id`:-`Network Code`) %>%
    dplyr::select(Station, dplyr::everything())

}
