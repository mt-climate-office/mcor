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
#' Variables and their associated values are in variable/value pairs
#' in the format "VARIABLE::value" (for example, "WTEQ::normal_1981"
#' will give the 1981--2010 normal value for Snow Water Equivalent).
#' Possible variables include:
#'
#' * **TAVG** --- Daily average air temperature (ºF)
#' * **TMAX** --- Daily maximum air temperature (ºF)
#' * **TMIN** --- Daily minimum air temperature (ºF)
#' * **TOBS** --- Instantaneously observed air temperature at start of day (ºF)
#' * **BATT** --- Battery voltage at start of day (volts)
#' * **BATX** --- Daily maximum battery voltage (volts)
#' * **PREC** --- Water year accumulated precipitation at start of day (inches)
#' * **PRCP** --- Daily total precipitation (inches)
#' * **PRCPSA** --- Daily total precipitation, snow-adjusted (inches)
#' * **SNWD** --- Total snow depth at start of day (inches)
#' * **WTEQ** --- Snow water equivalent at start of day (inches)
#' * **WTEQX** --- Daily maximum snow water equivalent (inches)
#' * **SNDN** --- Snow density at start of day (percent)
#' * **SNRR** --- Daily snow rain ratio (unitless)
#'
#' Not all values are available for all variables. Possible values include:
#' * **value** --- The measured value
#' * **qcFlag** --- Quality control flag
#' * **qaFlag** --- Quality assurance flag
#' * **prevValue** --- Previous year's value
#' * **delta** --- Delta, change from previous year's value
#' * **collectionDate** --- Collection date
#' * **normal_1981** --- Normal (1981-2010)
#' * **pctOfNormal_1981** --- % of normal (1981-2010)
#' * **average_1981** --- Average (1981-2010)
#' * **pctOfAverage_1981** --- % of average (1981-2010)
#' * **median_1981** --- Median (1981-2010)
#' * **pctOfMedian_1981** --- % of median (1981-2010)
#' * **normal_1971** --- Normal (1971-2000)
#' * **pctOfNormal_1971** --- % of normal (1971-2000)
#'
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
