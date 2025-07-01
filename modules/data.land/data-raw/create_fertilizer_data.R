library(readr)
library(dplyr)
library(stringr)

get_swat_fert_table <- function() {
  fertilizer.frt <- "https://raw.githubusercontent.com/swat-model/swatplus/main/data/Ames_sub1/fertilizer.frt"
  read_table(
    file = fertilizer.frt,
    skip = 1,
    col_types = cols(.default = col_character())
  ) |>
    mutate(across(c(min_n, min_p, org_n, org_p, nh3_n), as.numeric)) |>
    rename(
      fertilizer_name = name,
      fraction_mineral_n = min_n,
      fraction_organic_n = org_n,
      fraction_nh3_n = nh3_n
    ) |>
    mutate(
      swat_id = row_number(),
      fraction_no3_n = fraction_organic_n - fraction_nh3_n
    ) |>
    select(swat_id, fertilizer_name, fraction_mineral_n, fraction_nh3_n, 
           fraction_no3_n, fraction_organic_n, description)
}

fertilizer_nutrient_data <- get_swat_fert_table()

usethis::use_data(fertilizer_nutrient_data, overwrite = TRUE)