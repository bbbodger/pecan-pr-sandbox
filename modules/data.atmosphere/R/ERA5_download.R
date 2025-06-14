#' Download ERA5 Climate Data from the Copernicus CDS API
#'
#' @description
#' Download ERA5 climate data from the Copernicus Climate Data Store (CDS) API as NetCDF files, year by year, according to user-specified parameters.
#' The function saves one NetCDF file per year in the specified output directory.
#'
#' @details
#' This function requires a valid CDS API key and the Python `cdsapi` package installed and accessible via the `reticulate` package in R.
#' If you do not have a `.cdsapirc` file with your API credentials, set `auto.create.key = TRUE` to be prompted for your CDS API URL and key.
#' To get a Copernicus CDS API key, register at \url{https://cds.climate.copernicus.eu/profile}.
#' The API URL is \url{https://cds.climate.copernicus.eu/api/v2}.
#'
#' @param outfolder Character. Directory where downloaded NetCDF files will be saved.
#' @param start_date character: the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
#' @param end_date character: the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
#' @param extent numeric: a vector of numbers contains the bounding box (formatted as xmin, xmax, ymin, ymax) (longitude and latitude in degrees).
#' @param variables character: a vector contains variables to be downloaded (e.g., c("2m_temperature","surface_pressure")).
#' @param time Character vector or NULL. Hours of the day to download (e.g., c("00:00", "12:00")). Default to NULL to download all hours.
#' @param dataset Character. Name of the CDS dataset to use (default: "reanalysis-era5-single-levels").
#' @param product_type Character. Product type to request from CDS (default: "ensemble_members").
#' @param auto.create.key Boolean: decide if we want to generate the CDS RC file if it doesn't exist, the default is TRUE.
#' @param timeout numeric: the maximum time (in seconds) allowed to download the data. The default is 36000 seconds.
#'
#' @return
#' A list where each element is a list containing:
#'   \item{file}{File path to the downloaded NetCDF file.}
#'   \item{host}{Host name where the file was downloaded.}
#'   \item{startdate}{Start date and time of the data in the file.}
#'   \item{enddate}{End date and time of the data in the file.}
#'   \item{mimetype}{MIME type of the file ("application/x-netcdf").}
#'   \item{formatname}{Format name ("ERA5_year.nc").}
#'
#' @examples
#' \dontrun{
#' era5_files <- download.ERA5_cds(
#'   outfolder = "D:/working/era5_func_test",
#'   start_date = "2020-01-01",
#'   end_date = "2022-12-31",
#'   extent = c(-72.2215, -72.1215, 42.4878, 42.5878),
#'   variables = c("2m_temperature","surface_pressure"),
#'   time = NULL,
#'   product_type = "reanalysis"
#' )
#' }
#' @export
#' 
#' @importFrom purrr %>%
#' @author Dongchen Zhang
download.ERA5_cds <- function(outfolder, start_date, end_date, 
                              extent, variables, time = NULL, dataset = "reanalysis-era5-single-levels",
                              product_type = "ensemble_members", auto.create.key = T, timeout = 36000) {
  
  # setup timeout for download.
  options(timeout=timeout)
  # convert arguments to CDS API specific arguments.
  years <- sort(unique(lubridate::year(seq(lubridate::date(start_date), lubridate::date(end_date), "1 year"))))
  months <- sort(unique(lubridate::month(seq(lubridate::date(start_date), lubridate::date(end_date), "1 month")))) %>% 
    purrr::map(function(d)sprintf("%02d", d))
  days <- sort(unique(lubridate::day(seq(lubridate::date(start_date), lubridate::date(end_date), "1 day")))) %>% 
    purrr::map(function(d)sprintf("%02d", d))
  
  # handle time argument: all hours if Null
  if (is.null(time)) {
    times <- sprintf("%02d:00", 0:23)
  } else {
    times <- time
  }

  # Format area for CDS API (North, West, South, East)
  area <- round(c(extent[4], extent[1], extent[3], extent[2]), 2)
  variables <- as.list(variables)
  #load cdsapi from python environment.
  tryCatch({
    cdsapi <- reticulate::import("cdsapi")
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "Failed to load `cdsapi` Python library. ",
      "Please make sure it is installed to a location accessible to `reticulate`.",
      "You should be able to install it with the following command: ",
      "`pip install --user cdsapi`.",
      "The following error was thrown by `reticulate::import(\"cdsapi\")`: ",
      conditionMessage(e)
    )
  })
  #define function for building credential file.
  #maybe as a helper function.
  getnetrc <- function (dl_dir) {
    netrc <- file.path(dl_dir, ".cdsapirc")
    if (file.exists(netrc) == FALSE ||
        any(grepl("https://cds.climate.copernicus.eu/api/v2",
                  readLines(netrc))) == FALSE) {
      netrc_conn <- file(netrc)
      writeLines(c(
        sprintf(
          "url: %s",
          getPass::getPass(msg = "Enter URL from the following link \n (https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key):")
        ),
        sprintf(
          "key: %s",
          getPass::getPass(msg = "Enter KEY from the following link \n (https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key):")
        )
      ),
      netrc_conn)
      close(netrc_conn)
      message(
        "A netrc file with your CDS Login credentials was stored in the output directory "
      )
    }
    return(netrc)
  }
  #check if the token exists for the cdsapi.
  if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")) & auto.create.key) {
    if ("try-error" %in% class(try(find.package("getPass")))) {
      PEcAn.logger::logger.info("The getPass pacakge is not installed for creating the API key.")
      return(NA)
    } else {
      getnetrc(Sys.getenv("HOME"))
    }
  } else if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")) & !auto.create.key) {
    PEcAn.logger::logger.severe(
      "Please create a `${HOME}/.cdsapirc` file as described here:",
      "https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key ."
    )
  }
  #grab the client object.
  tryCatch({
    c <- cdsapi$Client()
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "The following error was thrown by `cdsapi$Client()`: ",
      conditionMessage(e)
    )
  })
  # loop over years.
  nc.paths <- c()
  for (y in years) {
    fname <- file.path(outfolder, paste0("ERA5_", y, ".nc"))
    # start retrieving data.
    # you need to have an account for downloaing the files
    # Read the documantion for how to setup your account and settings before trying this
    # https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5#HowtodownloadERA5-3-DownloadERA5datathroughtheCDSAPI
    c$retrieve(
      'reanalysis-era5-single-levels',
      list(
        'product_type' = list(product_type),
        'data_format' = 'netcdf',
        "download_format" = "unarchived",
        'day' = days,
        'time' = times,
        'month' = months,
        'year' = list(as.character(y)),
        "area" = area,
        'variable' = variables
      ),
      fname
    )

    # store the path.
    nc.paths <- c(nc.paths, fname)

  }
  # construct results to meet the requirements of pecan.met workflow.
  results <- vector("list", length = length(years))
  for (i in seq_along(results)) {
    results[[i]] <- list(file = nc.paths[i],
                         host = PEcAn.remote::fqdn(),
                         startdate = paste0(paste(years[i], months[1], days[1], sep = "-"), " ", times[1], ":00"),
                         enddate = paste0(paste(years[i], months[length(months)], days[length(days)], sep = "-"), " ", times[length(times)], ":00"),
                         mimetype = "application/x-netcdf",
                         formatname = "ERA5_year.nc")
  }
  return(results)
}