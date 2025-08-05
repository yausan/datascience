library(tidyverse)
library(readr)
library(stringr)

# Step 1: Load datasets
cleanBroadband <- read_csv("Cleaned_Data/cleanBroadband.csv")
Postcode_to_LSOA_clean <- read_csv("Cleaned_Data/Postcode_to_LSOA_clean.csv")

# Step 2: Clean Postcode formats
cleanBroadband <- cleanBroadband %>%
  mutate(Postcode = str_replace_all(Postcode, " ", ""))

Postcode_to_LSOA_clean <- Postcode_to_LSOA_clean %>%
  mutate(postcode_nospace = str_replace_all(Postcode, " ", ""))

# Step 3: Inner join on cleaned postcode
broadband_with_district <- cleanBroadband %>%
  inner_join(Postcode_to_LSOA_clean, by = c("Postcode" = "postcode_nospace"))

# Step 4: Handle duplicate columns (if any)
broadband_with_district <- broadband_with_district %>%
  mutate(County = coalesce(County.x, County.y)) %>%
  select(-County.x, -County.y, -Postcode.y)

# Step 5: Drop unwanted columns and reorder
final_data <- broadband_with_district %>%
  select(-c(
    Connections_under_10,
    `% of premises unable to receive 2Mbit/s`,
    `% of premises unable to receive 5Mbit/s`,
    `% of premises unable to receive 10Mbit/s`,
    `% of premises unable to receive 30Mbit/s`,
    `FTTP availability (% premises)`
  )) %>%
  select(Postcode, County, District, Town, LSOA_Code, MSOA_Name,
         `Average download speed (Mbit/s)`, `Average upload speed (Mbit/s)`, everything())

# Step 6: Save the final cleaned dataset
write_csv(final_data, "Cleaned_Data/broadband_summary_join_with_postcode_to_lsoa.csv")
