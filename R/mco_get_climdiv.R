#' Download the US Climate Division Dataset for Montana
#'
#' @param elements A character vector of elements to extract.\cr
#' pcpn = Precipitation\cr
#' tmpc = Average Temperature\cr
#' pdsi = Palmer Drought Severity Index\cr
#' phdi = Palmer Hydrological Drought Index\cr
#' zndx = Palmer Z Index\cr
#' pmdi = Modified Palmer Drought Severity Index\cr
#' hddc = Heating Degree Days\cr
#' cddc = Cooling Degree Days\cr
#' tmax = Maximum Temperature\cr
#' tmin = Minimum Temperature\cr
#' sp01 = 1-month Standardized Precipitation Index\cr
#' sp02 = 2-month Standardized Precipitation Index\cr
#' sp03 = 3-month Standardized Precipitation Index\cr
#' sp06 = 6-month Standardized Precipitation Index\cr
#' sp09 = 9-month Standardized Precipitation Index\cr
#' sp12 = 12-month Standardized Precipitation Index\cr
#' sp24 = 24-month Standardized Precipitation Index\cr
#' @return A named data frame containing the climate division data.
#'
#' * **Division code** — the NOAA Climate Divisional Database (nCLIMDIV) identifier
#' * **Element** — a four-sharacter string representing the climate element
#' * **Year** — the integer year
#' * **Month** — the integer month
#' * **Value** — the double precision element value
#' @export
#' @importFrom magrittr %>%
#' @examples
#' mco_get_climdiv("pcpn")
mco_get_climdiv <- function(elements = c("pcpn","tmpc")){
  procdate <- readr::read_lines("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/procdate.txt")

  # element_codes <- tibble::tibble(Element = c("pcpn","tmpc",
  #                                             "pdsi","phdi", "zndx","pmdi",
  #                                             "hddc","cddc","tmax","tmin",
  #                                             "sp01","sp02","sp03","sp06","sp09","sp12","sp24"),
  #                                 `Element long` = c("Precipitation",
  #                                                    "Average Temperature",
  #                                                    "Palmer Drought Severity Index",
  #                                                    "Palmer Hydrological Drought Index",
  #                                                    "Palmer Z Index",
  #                                                    "Modified Palmer Drought Severity Index",
  #                                                    "Heating Degree Days",
  #                                                    "Cooling Degree Days",
  #                                                    "Maximum Temperature",
  #                                                    "Minimum Temperature",
  #                                                    "1-month Standardized Precipitation Index",
  #                                                    "2-month Standardized Precipitation Index",
  #                                                    "3-month Standardized Precipitation Index",
  #                                                    "6-month Standardized Precipitation Index",
  #                                                    "9-month Standardized Precipitation Index",
  #                                                    "12-month Standardized Precipitation Index",
  #                                                    "24-month Standardized Precipitation Index"),
  #                                 `Element numeric` = c("01","02",
  #                                          "05","06","07","08",
  #                                          "25","26","27","28",
  #                                          "71","72","73","74","75","76","77"))

  elements %>%
    purrr::map(function(element){
      readr::read_fwf(stringr::str_c("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-",
                                     element,
                                     "dv-v1.0.0-",
                                     procdate),
                      col_positions = readr::fwf_positions(start = c(seq(1,7,2),
                                                              seq(11,88,7)),
                                                    end = c(seq(2,6,2),
                                                            10,
                                                            seq(17,94,7)),
                                                    col_names = c("State code",
                                                                  "Division code",
                                                                  "Element",
                                                                  "Year",
                                                                  1:12)),
                      col_types = "cccidddddddddddd") %>%
        dplyr::filter(`State code` == "24") %>% # Montana's state code
        dplyr::select(-`State code`) %>%
        dplyr::mutate(Element = element) %>%
        tidyr::gather(key = "Month",
                      value = "Value",
                      -`Division code`,
                      -Element,
                      -Year) %>%
        dplyr::mutate(Month = as.integer(Month))
    }) %>%
    dplyr::bind_rows()
}
