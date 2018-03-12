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
mco_get_swe_basins <- function(date = "latest",
                               huc = 6,
                               min_stations = 3){

  if(is.character(date) && date == "latest")
    date <- Sys.Date()

  snotel_inventory <-
    mco_get_snotel_inventory(date = date) %>%
    sf::st_transform(mt_state_plane) %>%
    dplyr::filter(`Start Date` <= as.Date("1981-01-01")) %>%
    dplyr::mutate(station = paste0(`Station Id`,":",
                                   `State Code`,":",
                                   `Network Code`))

  snotel_inventory <- suppressWarnings(readr::read_csv("https://www.wcc.nrcs.usda.gov/ftpref/data/water/wcs/gis/data/getdata/hucassociations.csv",
                                                       col_names = c("station", 1:6))) %>%
    dplyr::filter(grepl("SNTL",station),
                  station %in% snotel_inventory$station) %>%
    tidyr::gather(key = "Priority",
                  value = "WBD code",
                  -station) %>%
    dplyr::mutate(`WBD code` = stringr::str_sub(`WBD code`,1,huc)) %>%
    dplyr::filter(`WBD code` %in% (mt_watersheds_simple %>%
                                     dplyr::filter(`Hydrologic Unit` %in% c(huc)) %$%
                                     `WBD code`)) %>%
    dplyr::distinct() %>%
    dplyr::left_join(snotel_inventory,
                     by = "station")

  # https://wcc.sc.egov.usda.gov/reportGenerator/
  snotel_data <- readr::read_csv(paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport/daily/start_of_period/",
                                        paste0(snotel_inventory$station %>% unique,
                                               collapse = "%7C"),
                                        "%7C","state=%22MT%22",
                                        "%20AND%20",
                                        "network=%22SNTL%22",
                                        "%20AND%20element=%22WTEQ%22",
                                        "%20AND%20outServiceDate=%222100-01-01%22",
                                        "%7Cname/",date,",",date,"/stationId,",
                                        "WTEQ::value,",
                                        "WTEQ::median_1981,",
                                        "?fitToScreen=false"),
                                 comment = "#",
                                 col_types = readr::cols(
                                   `Date` = readr::col_date(format = ""),
                                   `Station Id` = readr::col_integer(),
                                   `Snow Water Equivalent (in) Start of Day Values` = readr::col_double(),
                                   `Median Snow Water Equivalent (1981-2010) (in) Start of Day Values` = readr::col_double()
                                 ))

  out <- snotel_inventory %>%
    dplyr::left_join(snotel_data,
                     by = c("Station Id")) %>%
    stats::na.omit() %>%
    dplyr::select(`WBD code`,
                  `Snow Water Equivalent (in) Start of Day Values`,
                  `Median Snow Water Equivalent (1981-2010) (in) Start of Day Values`) %>%
    dplyr::group_by(`WBD code`) %>%
    dplyr::summarise(`Stations Count` = n(),
                     `SWE (in)` = mean(`Snow Water Equivalent (in) Start of Day Values`),
                     `SWE 1981-2010 Median (in)` = mean(`Median Snow Water Equivalent (1981-2010) (in) Start of Day Values`)) %>%
    dplyr::filter(`Stations Count` >= min_stations) %>%
    dplyr::mutate(`Percent SWE` = round(100 * `SWE (in)`/`SWE 1981-2010 Median (in)`)) %>%
    dplyr::left_join(mt_watersheds_simple) %>%
    sf::st_as_sf() %>%
    sf::st_intersection(mt_counties_simple %>%
                          sf::st_union())

  out %>%
    dplyr::filter((out %>%
                     sf::st_area()) > units::set_units(5000000, m^2)) %>%
    dplyr::select(-`Hydrologic Unit`) %>%
    dplyr::select(`WBD code`, Watershed, dplyr::everything()) %>%
    sf::st_as_sf()
}
