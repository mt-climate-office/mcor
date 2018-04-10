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
                         "Date",
                         "DOY",
                         "Station"))

#' Download and process the Montana SNOTEL site inventory from the
#' NRCS National Water and Climate Center
#'
#' @param stations A character vector of station identifiers in trinomial format (e.g., '1040:CO:SNTL').
#'
#' @return A simple feature collection with 4 fields:
#' * **Station Id** — the SNOTEL station identification number
#' * **Station Name** — the SNOTEL station location name
#' * **State Code** — the two letter state code
#' * **`Network Code`** — the code for the station network
#' * **Start Date** — the date the stations started recording data
#'
#' @export
#' @importFrom magrittr  %<>% %>% %$%
#' @importFrom stats quantile
#' @examples
#' \dontrun{
#' mco_get_swe_normals()
#' }
mco_get_swe_normals <- function(stations = NULL){


  if(is.null(stations))
    stations <- mco_get_snotel_inventory()$station

  stations %<>%
    unique()

  if(length(stations) > 250)
    stations <- split(stations, ceiling(seq_along(stations)/250))

  stations %>%
    purrr::map_dfr(.f = function(x){

      mco_get_snotel_data(stations = x,
                          variables = c('WTEQ::value'),
                          start_date = '1981-01-01',
                          end_date = '2010-12-31')

    }) %>%
    dplyr::mutate(DOY = lubridate::yday(Date) %>%
                    as.integer())  %>%
    dplyr::filter(DOY != 366) %>%
    dplyr::group_by(DOY,Station) %>%
    dplyr::arrange(DOY,Station) %>%
    dplyr::summarise(mean = mean(`Snow Water Equivalent (in) Start of Day Values`, na.rm = TRUE),
                     `0%` = quantile(`Snow Water Equivalent (in) Start of Day Values`, probs = 0, na.rm = TRUE),
                     `10%` = quantile(`Snow Water Equivalent (in) Start of Day Values`, probs = 0.1, na.rm = TRUE),
                     `30%` = quantile(`Snow Water Equivalent (in) Start of Day Values`, probs = 0.3, na.rm = TRUE),
                     `50%` = quantile(`Snow Water Equivalent (in) Start of Day Values`, probs = 0.5, na.rm = TRUE),
                     `70%` = quantile(`Snow Water Equivalent (in) Start of Day Values`, probs = 0.7, na.rm = TRUE),
                     `90%` = quantile(`Snow Water Equivalent (in) Start of Day Values`, probs = 0.9, na.rm = TRUE),
                     `100%` = quantile(`Snow Water Equivalent (in) Start of Day Values`, probs = 1, na.rm = TRUE))

}
