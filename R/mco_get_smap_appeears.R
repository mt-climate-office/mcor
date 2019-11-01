utils::globalVariables(c("mt_state_simple",
                         "Division code",
                         "Element",
                         "Year",
                         "Month"))

#' Download the NASA Soil Moisture Active Passive (SMAP) data for Montana
#'
#' @param id A SMAP dataset to extract.\cr
#' \describe{
#' \item{SPL3FTA}{Radar Northern Hemisphere Daily Freeze/Thaw State}
#' \item{SPL3SMA}{Radar Global Daily Soil Moisture}
#' \item{SPL3SMP}{Radiometer Global Soil Moisture}
#' \item{SPL3SMAP}{Radar/Radiometer Global Soil Moisture}
#' \item{SPL4SMAU}{Surface/Rootzone Soil Moisture Analysis Update}
#' \item{SPL4SMGP}{Surface/Rootzone Soil Moisture Geophysical Data}
#' \item{SPL4SMLM}{Surface/Rootzone Soil Moisture Land Model Constants}
#' \item{SPL4CMDL}{Carbon Net Ecosystem Exchange}
#' }
#' @param group The SMAP dataset group name to extract.
#' @param name The SMAP dataset variable name to extract.
#' @param start_date An object of class Date or a character string formatted as
#' %Y-%m-%d (e.g., "2016-04-01") which specifies the start date of the download.
#' @param start_date An object of class Date or a character string formatted as
#' %Y-%m-%d (e.g., "2016-04-01") which specifies the end date of the download.
#' @param raw_dir A directory in which to download the raw SMAP data. If missing,
#' data are downloaded to a temporary directory.
#'
#' @return A raster brick of the desired SMAP dataset.
#'
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' mco_get_smap_appeears()
#' }
mco_get_smap_appeears <- function(id = "SPL4SMGP",
                                  group = "Geophysical_Data",
                                  name = "sm_rootzone",
                                  start_date = "latest",
                                  end_date = "latest",
                                  raw_dir = NULL){

  if(Sys.getenv("ed_un") == ""){
    stop("
This function requires a username and password from NASA's Earthdata portal.
If you have a username and password, pass them in as environment vars using:
Sys.setenv(ed_un = '<your username>', ed_pw = '<your password>')\n
If you do not yet have a username and password, register for one here:
https://urs.earthdata.nasa.gov/")
  }

  if(is.null(raw_dir)){
    raw_dir <- tempdir()
  }

  version <- smapr:::https_prefix() %>%
    smapr:::get_dir_contents() %>%
    stringr::str_subset(id) %>%
    stringr::str_extract('[^.]*$') %>%
    as.integer() %>%
    max()

  latest_date <- suppressWarnings(smapr:::route_to_dates(id, version) %>%
                                    smapr:::get_dir_contents() %>%
                                    lubridate::as_date() %>%
                                    max(na.rm = T)) - 1

  if(start_date == "latest" || start_date > latest_date){
    start_date <- latest_date
  }

  if(end_date == "latest" || end_date > latest_date){
    end_date <- latest_date
  }

  secret <- openssl::base64_encode(paste(Sys.getenv("ed_un"),
                                         Sys.getenv("ed_pw"),
                                         sep = ":"))
  token_response <- httr::POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/login",
                               httr::add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                                                 "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                               body = "grant_type=client_credentials") %>%
    httr::content()

  task_name <- ifelse(start_date==end_date,
                      paste0("montana_",start_date),
                      paste0("montana_",start_date,"–",end_date))

  out_dir <- paste0(raw_dir,"/",task_name)

  if(!dir.exists(out_dir)){
    dir.create(out_dir,
               showWarnings = FALSE,
               recursive = TRUE)

    # create the task request
    task <- list(task_type = 'area',
                 task_name = task_name,
                 params = list(
                   dates = list(
                     list(
                       startDate = start_date,
                       endDate = end_date
                     )
                   ),
                   layers = list(
                     list(
                       product = paste0(id, ".00", version),
                       layer = paste0(group, "_", name)
                     )
                   ),
                   geo = list(
                     features = list(
                       list(
                         geometry = mcor::mt_state_simple %>%
                           sf::st_buffer(10000) %>%
                           sf::st_transform(4326) %>%
                           geojsonsf::sf_geojson() %>%
                           jsonlite::fromJSON()
                       )
                     )
                   ),
                   output = list(format = list(type = "geotiff"),
                                 projection = "native"))) %>%
      jsonlite::toJSON(auto_unbox = TRUE)

    task_response <- httr::POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/task",
                                body = task,
                                encode = "json",
                                httr::add_headers(Authorization = paste("Bearer", token_response$token),
                                                  "Content-Type" = "application/json")) %>%
      httr::content()


    # Status
    status <- httr::GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/task/", task_response$task_id, sep = ""),
                        httr::add_headers(Authorization = paste("Bearer", token_response$token),
                                          "Content-Type" = "application/json")) %>%
      httr::content()

    while(status$status != "done"){

      Sys.sleep(0.5)

      status <- httr::GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/task/", task_response$task_id, sep = ""),
                          httr::add_headers(Authorization = paste("Bearer", token_response$token),
                                            "Content-Type" = "application/json")) %>%
        httr::content()

    }


    # Output
    bundle <- httr::GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/",
                              task_response$task_id, sep = ""),
                        httr::add_headers(Authorization = paste("Bearer", token_response$token),
                                          "Content-Type" = "application/json")) %>%
      httr::content() %$%
      files %>%
      purrr::map_dfr(tibble::as_tibble) %>%
      dplyr::mutate(file_name = file_name %>%
                      basename())

    manifest <- bundle %>%
      dplyr::filter(file_type == "csv") %$%
      readr::read_csv(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/",
                             task_response$task_id, '/',
                             file_id))

    download_smap <- function(id, name){
      httr::GET(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/",
                       task_response$task_id, '/',
                       id),
                httr::write_disk(paste0(out_dir,"/",name),
                                 overwrite = TRUE),
                httr::progress(),
                httr::add_headers(Authorization = paste("Bearer", token_response$token),
                                  "Content-Type" = "application/json"))
      return(paste0(out_dir,"/",name))
    }

    manifest$`File Name` %>%
      stringr::str_replace("_",".")

    bundle %<>%
      dplyr::left_join(manifest %>%
                         dplyr::select(`File Name`,
                                       Date) %>%
                         dplyr::mutate(`File Name` = `File Name` %>%
                                         stringr::str_replace("_",
                                                              ".") %>%
                                         stringr::str_c(".tif")),
                       by = c("file_name" = "File Name")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(local_file = download_smap(id = file_id,
                                               name = file_name)) %>%
      dplyr::ungroup()
  }

  out_dir %>%
    list.files(full.names = TRUE,
               pattern = "csv") %>%
    readr::read_csv() %>%
    dplyr::select(`File Name`,
                  Date) %>%
    dplyr::mutate(`File Name` = `File Name` %>%
                    stringr::str_replace("_",
                                         ".") %>%
                    stringr::str_c(".tif")) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(Raster = list(raster::stack(paste0(out_dir,"/",`File Name`)) %>%
                                     raster::mean())
    ) %$%
    {Raster %>%
        magrittr::set_names(Date)} %>%
    raster::brick()

}
