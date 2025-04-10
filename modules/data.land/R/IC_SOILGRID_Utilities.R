#' SoilGrids Initial Conditions (IC) Utilities
#' 
#' @author Akash
#' @description Functions for generating soil carbon IC files from SoilGrids250m data
#' @details This module provides functions for extracting, processing, and generating
#'          ensemble members for soil carbon initial conditions using SoilGrids data.
#'          All soil carbon values are in kg/m².

# Required package
library(truncnorm)  

#' Process SoilGrids data for initial conditions
#' 
#' @param settings PEcAn settings list containing site information. Should include:
#'        \itemize{
#'          \item settings$run$site - Site information with id, lat, lon
#'          \item settings$ensemble$size - (Optional) Number of ensemble members to create
#'          \item settings$soil$default_soilC - (Optional) Default soil carbon value in kg/m²
#'          \item settings$soil$default_uncertainty - (Optional) Default uncertainty as fraction
#'        }
#' @param csv_path Path to a CSV file containing site information (optional)
#' @param dir Output directory for IC files
#' @param overwrite Overwrite existing files? (Default: FALSE)
#' @param verbose Print detailed progress information to the terminal? TRUE/FALSE
#' 
#' @return List of paths to generated IC files
#' @export 
#' 
#' @details This function processes SoilGrids data to create carbon initial condition
#'          files. It extracts soil carbon data for all sites, handles missing values,
#'          generates ensemble members, and writes NetCDF files.
#' 
#' @examples
#' \dontrun{
#' # From settings object
#' settings <- PEcAn.settings::read.settings("pecan.xml")
#' ic_files <- soilgrids_ic_process(settings, dir = "output/IC/")
#'
#' # From CSV file
#' ic_files <- soilgrids_ic_process(csv_path = "sites.csv", dir = "output/IC/")
#' }
soilgrids_ic_process <- function(settings, csv_path=NULL, dir, overwrite = FALSE, verbose = FALSE) {
  # Start timing
  start_time <- proc.time()
  
  # Extract site information using PEcAn.settings::get.site.info
  site_info <- PEcAn.settings::get.site.info(settings = settings, csv_path = csv_path)
  
  # Get optional parameters from settings if available
  ensemble_size <- ifelse(is.null(settings$ensemble$size), 1, settings$ensemble$size)
  default_soilC <- ifelse(is.null(settings$soil$default_soilC), 5.0, settings$soil$default_soilC)
  default_uncertainty <- ifelse(is.null(settings$soil$default_uncertainty), 0.2, settings$soil$default_uncertainty)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(dir)) {
    PEcAn.logger::logger.info(sprintf("Creating output directory: %s", dir))
    dir.create(dir, recursive = TRUE)
  }
  
  # Create a data folder for intermediate outputs
  data_dir <- file.path(dir, "SoilGrids_data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  # Log the number of sites being processed
  n_sites <- nrow(site_info)
  PEcAn.logger::logger.info(sprintf("Processing %d site(s)", n_sites))
  
  if (verbose) {
    for (i in 1:nrow(site_info)) {
      PEcAn.logger::logger.info(sprintf("Site %d: %s (lat=%f, lon=%f)", 
                                       i, site_info$site_name[i],
                                       site_info$lat[i], site_info$lon[i]))
    }
  }
  
  # Check for cached data
  soilc_csv_path <- file.path(data_dir, "soilgrids_soilC_data.csv")
  if (file.exists(soilc_csv_path) && !overwrite) {
    PEcAn.logger::logger.info("Using existing SoilGrids data:", soilc_csv_path)
    soil_data <- utils::read.csv(soilc_csv_path, check.names = FALSE)
  } else {
    # Extract data for all sites at once
    PEcAn.logger::logger.info("Extracting SoilGrids data for", nrow(site_info), "sites")
    soil_data <- PEcAn.data.land::soilgrids_soilC_extract(
      site_info = site_info,
      outdir = data_dir,
      verbose = verbose
    )
    
    # Save the extracted data for future use
    utils::write.csv(soil_data, soilc_csv_path, row.names = FALSE)
  }
  
  # Validate soil carbon data units through range check
  if (any(soil_data$`Total_soilC_0-30cm` > 150, na.rm = TRUE)) {
    PEcAn.logger::logger.warn("Some soil carbon values exceed 150 kg/m², values may be in wrong units")
  }
  
  # Preprocess data
  PEcAn.logger::logger.info("Preprocessing soil carbon data")
  processed_data <- preprocess_soilgrids_data(
    soil_data = soil_data, 
    default_soilC = default_soilC,
    default_uncertainty = default_uncertainty,
    verbose = verbose
  )
  
  # Create a list to hold the ensemble files for each site
  all_ensemble_files <- list()
  
  # Process each site
  for (s in 1:nrow(site_info)) {
    current_site <- site_info[s, ]
    
    # Create output directory for this site
    site_outfolder <- file.path(dir, paste0("SoilGrids_site_", current_site$str_id))
    if (!dir.exists(site_outfolder)) {
      dir.create(site_outfolder, recursive = TRUE)
    }
    
    # Check for existing files
    existing_files <- list.files(site_outfolder, "*.nc$", full.names = TRUE)
    if (length(existing_files) > 0 && !overwrite) {
      PEcAn.logger::logger.info(sprintf("Using existing SoilGrids IC files for site %s", current_site$site_name))
      all_ensemble_files[[current_site$str_id]] <- existing_files
      next
    }
    
    if (verbose) {
      PEcAn.logger::logger.info(sprintf("Generating ensemble members for site %s", current_site$site_name))
    }
    
    # Generate ensemble members for this site
    ensemble_data <- generate_soilgrids_ensemble(
      processed_data = processed_data,
      site_id = current_site$site_id,
      lat = current_site$lat,
      lon = current_site$lon,
      ensemble_size = ensemble_size,
      verbose = verbose
    )
    
    # Write ensemble members to NetCDF files
    site_ensemble_files <- list()
    
    for (ens in seq_len(ensemble_size)) {
      # Write to NetCDF
      result <- PEcAn.data.land::pool_ic_list2netcdf(
        input = ensemble_data[[ens]],
        outdir = site_outfolder,
        siteid = current_site$site_id,
        ens = ens
      )
      
      site_ensemble_files[[ens]] <- result$file
      
      if (verbose) {
        PEcAn.logger::logger.info(sprintf("Generated IC file: %s for site %s", 
                                        basename(result$file), 
                                        current_site$site_name))
      }
    }
    
    # Add this site's files to the overall list
    all_ensemble_files[[current_site$str_id]] <- site_ensemble_files
  }
  
  # Log performance metrics
  end_time <- proc.time()
  elapsed_time <- end_time - start_time
  PEcAn.logger::logger.info(sprintf("IC generation completed for %d site(s) in %.2f seconds", 
                                  n_sites, elapsed_time[3]))
  
  return(all_ensemble_files)
}

#' Preprocess SoilGrids data
#' 
#' @param soil_data Raw soil carbon data from soilgrids_soilC_extract
#' @param default_soilC Default soil carbon value in kg/m² to use when data is missing
#' @param default_uncertainty Default uncertainty as fraction to use when data is missing
#' @param verbose Print detailed progress information to the terminal? TRUE/FALSE
#' 
#' @return Processed soil carbon data
#' @export
preprocess_soilgrids_data <- function(soil_data, default_soilC = 5.0, 
                                     default_uncertainty = 0.2, verbose = FALSE) {
  if (verbose) {
    PEcAn.logger::logger.info("Preprocessing soil carbon data")
  }
  
  # Create a copy to avoid modifying the original
  processed <- soil_data
  
  # Handle missing values in Total_soilC_0-30cm 
  na_count <- sum(is.na(processed$`Total_soilC_0-30cm`))
  if (na_count > 0) {
    PEcAn.logger::logger.warn(sprintf("Found %d missing values in soil carbon data", na_count))
    
    # Sites with missing 0-30cm but available 0-200cm data
    has_200cm_data <- is.na(processed$`Total_soilC_0-30cm`) & !is.na(processed$`Total_soilC_0-200cm`)
    if (any(has_200cm_data)) {
      processed$`Total_soilC_0-30cm`[has_200cm_data] <- processed$`Total_soilC_0-200cm`[has_200cm_data] * 0.15
      PEcAn.logger::logger.warn(sprintf(
        "Using scaled 0-200cm soil carbon values for %d site(s)", sum(has_200cm_data)
      ))
      
      if (verbose) {
        for (i in which(has_200cm_data)) {
          PEcAn.logger::logger.debug(sprintf(
            "Using scaled 0-200cm soil carbon value (%.2f) for site %s", 
            processed$`Total_soilC_0-30cm`[i], processed$Site_ID[i]
          ))
        }
      }
    }
    
    # Sites still with missing data - use default value
    still_missing <- is.na(processed$`Total_soilC_0-30cm`)
    if (any(still_missing)) {
      processed$`Total_soilC_0-30cm`[still_missing] <- default_soilC
      PEcAn.logger::logger.warn(sprintf(
        "Using default soil carbon value (%.2f kg/m²) for %d site(s)", 
        default_soilC, sum(still_missing)
      ))
      
      if (verbose) {
        for (i in which(still_missing)) {
          PEcAn.logger::logger.debug(sprintf(
            "Using default soil carbon value (%.2f kg/m²) for site %s", 
            default_soilC, processed$Site_ID[i]
          ))
        }
      }
    }
  }
  
  # Handle missing values in Std_soilC_0-30cm
  na_count <- sum(is.na(processed$`Std_soilC_0-30cm`))
  if (na_count > 0) {
    PEcAn.logger::logger.warn(sprintf("Found %d missing values in soil carbon uncertainty", na_count))
    
    # Sites with missing 0-30cm but available 0-200cm uncertainty data
    has_200cm_data <- is.na(processed$`Std_soilC_0-30cm`) & !is.na(processed$`Std_soilC_0-200cm`)
    if (any(has_200cm_data)) {
      processed$`Std_soilC_0-30cm`[has_200cm_data] <- processed$`Std_soilC_0-200cm`[has_200cm_data] * 0.15
      PEcAn.logger::logger.warn(sprintf(
        "Using scaled 0-200cm soil carbon uncertainty for %d site(s)", sum(has_200cm_data)
      ))
      
      if (verbose) {
        for (i in which(has_200cm_data)) {
          PEcAn.logger::logger.debug(sprintf(
            "Using scaled 0-200cm soil carbon uncertainty (%.2f) for site %s", 
            processed$`Std_soilC_0-30cm`[i], processed$Site_ID[i]
          ))
        }
      }
    }
    
    # Sites still with missing uncertainty - use default percentage of mean
    still_missing <- is.na(processed$`Std_soilC_0-30cm`)
    if (any(still_missing)) {
      processed$`Std_soilC_0-30cm`[still_missing] <- 
        processed$`Total_soilC_0-30cm`[still_missing] * default_uncertainty
      PEcAn.logger::logger.warn(sprintf(
        "Using default uncertainty (%.1f%% of mean) for %d site(s)", 
        default_uncertainty * 100, sum(still_missing)
      ))
      
      if (verbose) {
        for (i in which(still_missing)) {
          PEcAn.logger::logger.debug(sprintf(
            "Using default uncertainty (%.1f%% of mean) for site %s", 
            default_uncertainty * 100, processed$Site_ID[i]
          ))
        }
      }
    }
  }
  
  # Ensure standard deviation is non-negative 
  neg_sd_count <- sum(processed$`Std_soilC_0-30cm` < 0, na.rm = TRUE)
  if (neg_sd_count > 0) {
    PEcAn.logger::logger.warn(sprintf("Found %d negative standard deviations", neg_sd_count))
    processed$`Std_soilC_0-30cm` <- pmax(processed$`Std_soilC_0-30cm`, 0, na.rm = TRUE)
  }
  
  # Ensure mean is non-negative 
  neg_mean_count <- sum(processed$`Total_soilC_0-30cm` < 0, na.rm = TRUE)
  if (neg_mean_count > 0) {
    PEcAn.logger::logger.warn(sprintf("Found %d negative mean values", neg_mean_count))
    processed$`Total_soilC_0-30cm` <- pmax(processed$`Total_soilC_0-30cm`, 0, na.rm = TRUE)
  }
  
  # Add minimum standard deviation to avoid zero uncertainty 
  min_sd <- 0.1 * processed$`Total_soilC_0-30cm` # 10% of mean as minimum SD
  is_zero_sd <- processed$`Std_soilC_0-30cm` == 0 | is.na(processed$`Std_soilC_0-30cm`)
  zero_sd_count <- sum(is_zero_sd)
  
  if (zero_sd_count > 0) {
    PEcAn.logger::logger.info(sprintf("Setting minimum uncertainty for %d zero/NA standard deviations", 
                                     zero_sd_count))
    processed$`Std_soilC_0-30cm` <- pmax(processed$`Std_soilC_0-30cm`, min_sd, na.rm = TRUE)
  }
  
  return(processed)
}

#' Generate ensemble members for a site
#' 
#' @param processed_data Processed soil carbon data
#' @param site_id Site ID
#' @param lat Site latitude
#' @param lon Site longitude
#' @param ensemble_size Number of ensemble members to create
#' @param verbose Print detailed progress information to the terminal? TRUE/FALSE
#' 
#' @return List of ensemble data for the site
#' @export
generate_soilgrids_ensemble <- function(processed_data, site_id, lat, lon, ensemble_size, verbose = FALSE) {
  if (verbose) {
    PEcAn.logger::logger.info(sprintf("Generating %d ensemble members for site %s", ensemble_size, site_id))
  }
  
  # Get site row from processed data
  site_row <- which(processed_data$Site_ID == site_id)
  if (length(site_row) == 0) {
    PEcAn.logger::logger.severe(sprintf("Site %s not found in processed data", site_id))
  }
  
  # Set random seed for reproducibility
  set.seed(as.numeric(site_id))
  
  # Generate all ensemble members at once 
  soil_c_values <- truncnorm::rtruncnorm(
    n = ensemble_size,
    a = 0,  # Lower bound (no negative values)
    b = Inf,  # Upper bound
    mean = processed_data$`Total_soilC_0-30cm`[site_row],
    sd = processed_data$`Std_soilC_0-30cm`[site_row]
  )
  
  if (verbose) {
    PEcAn.logger::logger.debug(sprintf(
      "Generated %d soil carbon values for site %s (mean: %.2f, sd: %.2f)",
      ensemble_size,
      site_id,
      processed_data$`Total_soilC_0-30cm`[site_row],
      processed_data$`Std_soilC_0-30cm`[site_row]
    ))
  }
  
  # Create input lists for pool_ic_list2netcdf
  ensemble_data <- lapply(seq_len(ensemble_size), function(ens) {
    list(
    dims = list(
        lat = lat,
        lon = lon,
      time = 1
    ),
    vals = list(
        soil_organic_carbon_content = soil_c_values[ens],
        wood_carbon_content = 0,  # Not provided by SoilGrids
        litter_carbon_content = 0  # Not provided by SoilGrids
      )
    )
  })
  
  return(ensemble_data)
}
