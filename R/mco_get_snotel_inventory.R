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
                         'Station'))

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
mco_get_snotel_inventory <- function(){

  date <- Sys.Date()

  suppressWarnings(readr::read_csv(stringr::str_c('https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/start_of_period/',
                                          'network="SNTL"|name','/',
                                          date,',',date,'/',
                                          stringr::str_c(c('stationId',
                                                         'name',
                                                         'state.code',
                                                         'network.code',
                                                         'latitude',
                                                         'longitude',
                                                         'inServiceDate',
                                                         'outServiceDate'),
                                                         collapse = ',')),
                                     col_types = readr::cols(
                                       `Station Id` = readr::col_integer(),
                                       `Station Name` = readr::col_character(),
                                       `State Code` = readr::col_character(),
                                       `Network Code` = readr::col_character(),
                                       Latitude = readr::col_double(),
                                       Longitude = readr::col_double(),
                                       `Start Date` = readr::col_date(format = ""),
                                       `End Date` = readr::col_date(format = "")
                                     ),
                                     comment = "#")) %>%
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("Longitude","Latitude"),
                 crs = 4326) %>%
    dplyr::mutate(Station = paste0(`Station Id`,":",
                                   `State Code`,":",
                                   `Network Code`)) %>%
    dplyr::select(`Station`,dplyr::everything())

}
