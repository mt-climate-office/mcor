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
#' @param stations A character vector of station identifiers in trinomial format (e.g., '1040:CO:SNTL').
#' @param variables A character vector of variables to request.
#' @param start_date A [Date] object as the start date. Defaults to period of record.
#' @param end_date A [Date] object as the end date. Defaults to period of record.
#'
#' @return A data frame containing the requested variables.
#'
#' @export
#' @importFrom magrittr %>% %$%
#' @examples
#' \dontrun{
#' mco_get_snotel_data()
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
    dplyr::select(-`Station Id`,
                  -`State Code`,
                  -`Network Code`) %>%
    dplyr::select(Station, dplyr::everything())

}
