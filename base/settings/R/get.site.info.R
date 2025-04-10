#' Extract and validate site information from settings or CSV file
#'
#' @param settings PEcAn settings list containing site information (optional)
#' @param csv_path Path to a CSV file containing site information (optional)
#' @param strict_checking Logical. If TRUE, will validate coordinates more strictly
#'
#' @return A data frame with site_id, site_name, lat, lon, and str_id
#' @export get.site.info
#'
#' @details This function extracts and validates site information from either a PEcAn settings 
#'          object or a CSV file. At least one input must be provided. If both are provided,
#'          the settings object takes precedence.
#'          
#'          If using a CSV file, it must contain at minimum the columns: site_id, lat, and lon.
#'          The column site_name is optional and will default to site_id if not provided.
#'
#' @examples
#' \dontrun{
#' # From settings object
#' settings <- PEcAn.settings::read.settings("pecan.xml")
#' site_info <- PEcAn.settings::get.site.info(settings)
#'
#' # From CSV file
#' site_info <- PEcAn.settings::get.site.info(csv_path = "sites.csv")
#' }
get.site.info <- function(settings = NULL, csv_path = NULL, strict_checking = TRUE) {
  
  # Check if at least one input is provided
  if (is.null(settings) && is.null(csv_path)) {
    PEcAn.logger::logger.severe("No site information provided. Please provide either settings or csv_path.")
  }
  
  # Process settings object (highest precedence when both are provided)
  if (!is.null(settings)) {
    PEcAn.logger::logger.debug("Extracting site information from settings object")
    
    # Check if this is a MultiSettings object
    if (inherits(settings, "MultiSettings")) {
      PEcAn.logger::logger.info("Detected MultiSettings object")
      
      # Process sites from MultiSettings
      site_list <- lapply(settings, function(s) {
        if (is.null(s$run) || is.null(s$run$site)) {
          PEcAn.logger::logger.severe("Site information missing from one of the settings in MultiSettings")
        }
        return(s$run$site)
      })
    } else {
      # Process single settings object
      if (is.null(settings$run) || is.null(settings$run$site)) {
        PEcAn.logger::logger.severe("Site information missing from settings (settings$run$site)")
      }
      
      # Check if we have vectorized site information
      site_fields <- c("id", "name", "lat", "lon")
      field_lengths <- sapply(site_fields, function(f) {
        if (is.null(settings$run$site[[f]])) 0 else length(settings$run$site[[f]])
      })
      
      max_length <- max(field_lengths)
      is_vectorized <- max_length > 1
      
      if (is_vectorized) {
        PEcAn.logger::logger.info("Detected vectorized site information in settings")
        
        # Create a list of site information from vectorized input
        site_list <- list()
        for (i in 1:max_length) {
          site <- list()
          for (field in site_fields) {
            if (!is.null(settings$run$site[[field]]) && i <= length(settings$run$site[[field]])) {
              site[[field]] <- settings$run$site[[field]][i]
            }
          }
          site_list[[i]] <- site
        }
      } else {
        # Just a single non-vectorized site
        site_list <- list(settings$run$site)
      }
    }
  } else {
    # Process CSV file input
    PEcAn.logger::logger.debug("Reading site information from CSV file:", csv_path)
    
    # Check if file exists
    if (!file.exists(csv_path)) {
      PEcAn.logger::logger.severe("CSV file not found:", csv_path)
    }
    
    # Read CSV file
    csv_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
    
    # Check for required columns
    required_cols <- c("site_id", "lat", "lon")
    missing_cols <- setdiff(required_cols, colnames(csv_data))
    if (length(missing_cols) > 0) {
      PEcAn.logger::logger.severe("Missing required columns in CSV file: ", 
                                 paste(missing_cols, collapse = ", "))
    }
    
    # Add site_name if missing (use site_id as default)
    if (!"site_name" %in% colnames(csv_data)) {
      csv_data$site_name <- as.character(csv_data$site_id)
      PEcAn.logger::logger.debug("Added site_name column using site_id values")
    }
    
    # Convert CSV data to the site_list format for consistent processing
    site_list <- lapply(1:nrow(csv_data), function(i) {
      row <- csv_data[i, ]
      list(
        id = row$site_id,
        name = row$site_name,
        lat = row$lat,
        lon = row$lon
      )
    })
  }
  
  # Process each site from the site_list
  result <- lapply(seq_along(site_list), function(i) {
    site <- site_list[[i]]
    
    # Check for required site ID
    if (is.null(site$id)) {
      PEcAn.logger::logger.severe(sprintf("Site ID is required but missing for site %d", i))
    }
    
    # Extract and validate site ID
    site_id <- as.numeric(site$id)
    if (is.na(site_id)) {
      PEcAn.logger::logger.severe(sprintf("Site ID must be numeric for site %d", i))
    }
    
    # Check if the site name exists, use ID as name if missing
    site_name <- ifelse(!is.null(site$name), site$name, as.character(site_id))
    
    # Check for required coordinates
    if (is.null(site$lat) || is.null(site$lon)) {
      PEcAn.logger::logger.severe(sprintf("Site coordinates are required but missing for site %d", i))
    }
    
    # Extract and validate coordinates
    lat <- as.numeric(site$lat)
    lon <- as.numeric(site$lon)
    
    if (is.na(lat) || is.na(lon)) {
      PEcAn.logger::logger.severe(sprintf("Site coordinates must be numeric for site %d", i))
    }
    
    # site ID for display and file naming
    str_id <- as.character(site$id)
    
    # Return a standardized site info list
    return(list(
      site_id = site_id,
      site_name = site_name,
      lat = lat,
      lon = lon,
      str_id = str_id
    ))
  })
  
  # Create the data frame using vapply to maintain types
  site_df <- data.frame(
    site_id = vapply(result, function(x) x$site_id, numeric(1)),
    site_name = vapply(result, function(x) x$site_name, character(1)),
    lat = vapply(result, function(x) x$lat, numeric(1)),
    lon = vapply(result, function(x) x$lon, numeric(1)),
    str_id = vapply(result, function(x) x$str_id, character(1)),
    stringsAsFactors = FALSE
  )
  
  # Validate coordinates based on strictness settings
  if (strict_checking) {
    # Check for valid latitude range
    invalid_lats <- site_df$lat < -90 | site_df$lat > 90
    if (any(invalid_lats)) {
      invalid_sites <- paste(site_df$site_id[invalid_lats], collapse = ", ")
      PEcAn.logger::logger.severe(sprintf("Invalid latitude values (outside -90 to 90) found for sites: %s", invalid_sites))
    }
    
    # Check for valid longitude range
    invalid_lons <- site_df$lon < -180 | site_df$lon > 180
    if (any(invalid_lons)) {
      invalid_sites <- paste(site_df$site_id[invalid_lons], collapse = ", ")
      PEcAn.logger::logger.severe(sprintf("Invalid longitude values (outside -180 to 180) found for sites: %s", invalid_sites))
    }
  } else {
    # Just warn if coordinates are suspicious
    suspicious_lats <- site_df$lat < -90 | site_df$lat > 90
    if (any(suspicious_lats)) {
      suspicious_sites <- paste(site_df$site_id[suspicious_lats], collapse = ", ")
      PEcAn.logger::logger.warn(sprintf("Suspicious latitude values (outside -90 to 90) found for sites: %s", suspicious_sites))
    }
    
    suspicious_lons <- site_df$lon < -180 | site_df$lon > 180
    if (any(suspicious_lons)) {
      suspicious_sites <- paste(site_df$site_id[suspicious_lons], collapse = ", ")
      PEcAn.logger::logger.warn(sprintf("Suspicious longitude values (outside -180 to 180) found for sites: %s", suspicious_sites))
    }
  }
  
  return(site_df)
}