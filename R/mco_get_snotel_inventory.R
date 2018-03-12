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
#' @return A simple feature collection with 4 fields:
#' * **Station Id** — the SNOTEL station identification number
#' * **Station Name** — the SNOTEL station location name
#' * **State Code** — the two letter state code
#' * **`Network Code`** — the code for the station network
#' * **Start Date** — the date the stations started recording data
#'
#' @export
#' @importFrom magrittr %>% %$%
#' @examples
#' \dontrun{
#' mco_get_snotel_inventory()
#' }
mco_get_snotel_inventory <- function(date = Sys.Date()){

  suppressWarnings(readr::read_csv(paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/start_of_period/",
                                            "network=%22SNTL%22",
                                            "%20AND%20",
                                            "outServiceDate=%222100-01-01%22",
                                            "%7Cname/",date,",",date,"/stationId,",
                                            "name,",
                                            "state.code,",
                                            "network.code,",
                                            "latitude,",
                                            "longitude,",
                                            "inServiceDate",
                                            "?fitToScreen=false"),
                                     col_types = readr::cols(
                                       `Station Id` = readr::col_integer(),
                                       `Station Name` = readr::col_character(),
                                       `State Code` = readr::col_character(),
                                       `Network Code` = readr::col_character(),
                                       Latitude = readr::col_double(),
                                       Longitude = readr::col_double(),
                                       `Start Date` = readr::col_date(format = "")
                                     ),
                                     comment = "#")) %>%
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("Longitude","Latitude"),
                 crs = 4326)

}
