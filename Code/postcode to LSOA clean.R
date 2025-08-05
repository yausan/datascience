library(tidyverse)

# Load dataset
Postcode_to_LSOA <- read_csv("/Users/karma/Coursework_yausan35D_230486/Obtained_Data/LSOA_data/Postcode to LSOA 1.csv")

# Step 1: Select and rename relevant columns
Postcode_to_LSOA_clean <- Postcode_to_LSOA %>%
  select(
    pcds,              # Postcode
    lsoa11cd,          # LSOA code
    lsoa11nm,          # LSOA name (district level)
    ladnm,             # Local Authority (town/city)
    msoa11nm,          # MSOA name (mid-level area)
    ladcd              # Local authority district code
  ) %>%
  rename(
    Postcode = pcds,
    LSOA_Code = lsoa11cd,
    District = lsoa11nm,
    Town = ladnm,
    MSOA_Name = msoa11nm,
    District_Code = ladcd
  )

# Step 2: Remove rows with missing key data
Postcode_to_LSOA_clean <- Postcode_to_LSOA_clean %>%
  filter(!is.na(Postcode), !is.na(LSOA_Code), !is.na(Town))

# Step 3: Filter for South Yorkshire and West Yorkshire (if needed)
south_yorkshire_districts <- c("Barnsley", "Doncaster", "Rotherham", "Sheffield")
west_yorkshire_districts <- c("Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield")

Postcode_to_LSOA_clean <- Postcode_to_LSOA_clean %>%
  filter(Town %in% c(south_yorkshire_districts, west_yorkshire_districts))

# Step 4: Add County column based on Town
Postcode_to_LSOA_clean <- Postcode_to_LSOA_clean %>%
  mutate(County = case_when(
    Town %in% south_yorkshire_districts ~ "South Yorkshire",
    Town %in% west_yorkshire_districts ~ "West Yorkshire",
    TRUE ~ NA_character_
  ))

# Step 5: Standardize Postcode formatting
Postcode_to_LSOA_clean <- Postcode_to_LSOA_clean %>%
  mutate(Postcode = toupper(str_trim(Postcode)))

# Step 6: Remove duplicates
Postcode_to_LSOA_clean <- Postcode_to_LSOA_clean %>%
  distinct()

# Step 7: Save cleaned dataset
write_csv(Postcode_to_LSOA_clean, "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/Postcode_to_LSOA_clean.csv")

# Step 8: View cleaned data
View(Postcode_to_LSOA_clean)
