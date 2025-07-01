#' Calculate the Nitrogen and Carbon Content of a Fertilizer Application
#'
#' This function calculates the different forms of nitrogen (NO3-N, NH4-N, organic N) and organic carbon (C_org) in a fertilizer application.
#' It can determine fertilizer nitrogen and carbon content using either a lookup table based on 
#' the SWAT model's [`fertilizer.frt`](https://github.com/swat-model/swatplus/blob/main/data/Osu_1hru/fertilizer.frt)
#' file or determine the fertilizer's 
#' nutrient content based on NN-PP-KK format.
#'
#' Consistent with assumptions (TODO: confirm this statement) in DayCent, DSSAT, and other models, urea is treated as NH3 because the transformation typically occurs within a day.
#' 
#' @param fertilizer_type Character string specifying the type of fertilizer. Can be one of "urea", "ammonium_nitrate", "compost", "manure", etc., or a string in the format "NN-PP-KK" (e.g., "45-5-10").
#' @param fertilizer_amount Numeric value specifying the amount of fertilizer applied in kg/ha.
#' @param fraction_organic_n Optional numeric value specifying the fraction of nitrogen in the fertilizer. Used for organic fertilizers if not provided in the dataset.
#' @param fraction_organic_c Optional numeric value specifying the fraction of carbon in the fertilizer. Used for organic fertilizers if not provided in the dataset.
#'
#' @return A list containing:
#'   - `fertilizer_type`: The type of fertilizer used.
#'   - `NO3_N`: The amount of nitrate nitrogen (NO3-N) in kg/ha.
#'   - `NH4_N`: The amount of ammonium nitrogen (NH4-N) in kg/ha.
#'   - `N_org`: The amount of organic nitrogen in kg/ha.
#'   - `C_org`: The amount of organic carbon in kg/ha.
#'
#' @examples
#' fertilizer_components("45-00-00", 200)
#' fertilizer_components("compost", 1000)
#' fertilizer_components("manure", 1000, fraction_organic_n = 0.02, fraction_organic_c = 0.08)
#'
#' @import dplyr
#' @import tidyverse
#' @export
fertilizer_components <- function(
  fertilizer_type, 
  fertilizer_amount, 
  fraction_organic_n = NULL, 
  fraction_organic_c = NULL,
  cn_ratio = NULL) {
  
  load(fertilizer_data)
  # Validate input for organic fertilizers
  if (!is.null(fraction_organic_n) || !is.null(fraction_organic_c)) {
    if (is.null(fraction_organic_n) || is.null(fraction_organic_c)) {
      PEcAn.logger::logger.error("Both fraction_organic_n and fraction_organic_c must be provided if either is specified.")
    }
  }
  
  # Find the row with fertilizer_type
  fertilizer_info <- fertilizer_data |> 
    dplyr::filter(fertilizer_name == fertilizer_type)
  
  # Check if the fertilizer type exists
  if (nrow(fertilizer_info) == 0) {
    # If not in the database and fractions are provided, use the provided values
    if (!is.null(fraction_organic_n) && !is.null(fraction_organic_c)) {
      NO3_N <- 0
      NH4_N <- 0
      N_org <- (fertilizer_amount * fraction_organic_n)
      C_org <- (fertilizer_amount * fraction_organic_c)
      return(list(
        fertilizer_type = fertilizer_type,
        NO3_N = NO3_N,
        NH4_N = NH4_N,
        N_org = N_org,
        C_org = C_org
      ))
    }
    # If not in the database, check if the fertilizer type is in NN-PP-KK format (e.g., 45-5-10)
    if (stringr::str_detect(fertilizer_type, "^\\d{1,2}-\\d{1,2}-\\d{1,2}$")) {
      # Split NN-PP-KK format into components
      fraction_organic_n <- stringr::str_split(fertilizer_type, "-", simplify = TRUE)[1] |> 
        as.numeric() / 100 # convert % to fraction (0-1) 
      
      # Assume all nitrogen is in the form of NO3_N
      NO3_N <- (fertilizer_amount * fraction_organic_n)
      NH4_N <- 0
      N_org <- 0
      C_org <- 0
      return(list(
        fertilizer_type = fertilizer_type,
        NO3_N = NO3_N,
        NH4_N = NH4_N,
        N_org = N_org,
        C_org = C_org
      ))
    } else {
      PEcAn.logger::logger.error(
        "Invalid fertilizer type. Please choose a valid fertilizer_type or provide NN-PP-KK format.\n",
        "valid fertilizer_types include:", paste0(fertilizer_data$fertilizer_name, sep = ", ")
      )
    }
  } else {
    # If fractions are provided for a fertilizer type that is in the database, warn the user
    if (!is.null(fraction_organic_n) && !is.null(fraction_organic_c)) {
      PEcAn.logger::logger.warn("Provided fraction_organic_n and fraction_organic_c values are being ignored as the fertilizer type is found in the database.")
    }
    
    # Calculate the components directly in the data frame
    fertilizer_info <- fertilizer_info |> 
      dplyr::mutate(
        NO3_N = (fertilizer_amount * fraction_no3_n),
        NH4_N = (fertilizer_amount * fraction_nh3_n),
        N_org = (fertilizer_amount * fraction_organic_n),
        C_org = (fertilizer_amount * fraction_organic_c)
      )
    
    return(fertilizer_info |> 
             dplyr::select(fertilizer_name, NO3_N, NH4_N, N_org, C_org) |> 
             dplyr::rename(fertilizer_type = fertilizer_name) |> 
             as.list())
  }
}