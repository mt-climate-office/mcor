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
mco_get_snodas <- function(x = mt_state %>%
                             sf::st_buffer(10000),
                           date = "latest",
                           masked = TRUE,
                           out_dir = tempdir(),
                           overwrite = TRUE){


  if(masked){
    masked <- "masked"
  }else{
    masked <- "unmasked"
  }

  basedir <- paste0("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/",masked,"/")

  year <- curl::curl(basedir) %>%
    readr::read_table(col_names = FALSE) %$%
    max(X9)

  month <- paste0(basedir,year,"/") %>%
    curl::curl() %>%
    readr::read_table(col_names = FALSE) %$%
    max(X9)

  filename <- paste0(basedir,year,"/",month,"/") %>%
    curl::curl() %>%
    readr::read_table(col_names = FALSE) %$%
    max(X9)

  latest_filepath <- paste0(basedir,year,"/",month,"/",filename)
  latest_date <- filename %>%
    stringr::str_remove("SNODAS_") %>%
    stringr::str_remove(".tar") %>%
    lubridate::as_date()

  if(date == "latest"){
    filepath <- latest_filepath

  }else{
    date %<>%
      lubridate::as_date(date)

    if(date > latest_date){
      date <- latest_date
      filepath <- latest_filepath
    }else{
      filename <- paste0("SNODAS_",
                         format(date, "%Y%m%d"),
                         ".tar")
      filepath <- paste0(basedir,
                         lubridate::year(date),
                         "/",
                         format(date, "%m_%b") ,
                         "/",
                         filename
                         )

    }
  }

  temp <- tempfile() %T>%
    dir.create()

  destfile <- paste0(temp,"/",filename)

  curl::curl_download(filepath, destfile = destfile)

  untar(destfile,
        exdir = destfile)

  destfile


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
