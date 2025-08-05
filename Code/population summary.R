# Load required libraries
library(readr)
library(dplyr)
library(stringr)

# Step 1: Load the datasets
population <- read_csv("Obtained_Data/population/Population.csv")  # Has Postcode and Population columns
postcode_lsoa <- read_csv("Cleaned_Data/Postcode_to_LSOA_clean.csv")  # Includes full Postcode, District, and possibly County

# Step 2: Extract postcode sector from postcode_lsoa
postcode_lsoa <- postcode_lsoa %>%
  mutate(postcode_sector = str_sub(Postcode, 1, 6),
         postcode_sector = str_trim(postcode_sector))

# Step 3: Prepare population data
population <- population %>%
  rename(postcode_sector = Postcode) %>%
  mutate(postcode_sector = str_trim(postcode_sector))

# Step 4: Merge population with postcode_lsoa on postcode_sector
merged_data <- inner_join(population, postcode_lsoa, by = "postcode_sector")

# Step 5: Summarise total population by County and District
population_summary <- merged_data %>%
  group_by(County, District) %>%
  summarise(
    total_population = sum(Population, na.rm = TRUE),
    postcode_sector_count = n_distinct(postcode_sector),
    .groups = "drop"
  )

# Step 6: View and save the result
print(population_summary)

# Save to CSV
write_csv(population_summary, "Cleaned_Data/population_summary.csv")
