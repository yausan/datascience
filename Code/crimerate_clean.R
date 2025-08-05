library(tidyverse)
library(lubridate)

# Step 1: Load datasets
south_yorkshire <- read_csv("/Users/karma/Coursework_yausan35D_230486/Obtained_Data/crimerate/2025-05-south-yorkshire-street.csv")
west_yorkshire <- read_csv("/Users/karma/Coursework_yausan35D_230486/Obtained_Data/crimerate/2025-05-west-yorkshire-street.csv")

# Step 2: Inspect structure
glimpse(south_yorkshire)
glimpse(west_yorkshire)

# Step 3: Add County column
south_yorkshire <- south_yorkshire %>% mutate(County = "South Yorkshire")
west_yorkshire <- west_yorkshire %>% mutate(County = "West Yorkshire")

# ✅ Step 4: Extract District from LSOA name for both
# LSOA name format: "Barnsley 004A", "Leeds 001B", etc.
south_yorkshire <- south_yorkshire %>%
  mutate(District = str_extract(`LSOA name`, "^[A-Za-z\\s\\-]+") %>% str_trim())

west_yorkshire <- west_yorkshire %>%
  mutate(District = str_extract(`LSOA name`, "^[A-Za-z\\s\\-]+") %>% str_trim())

# Step 5: Combine datasets
combined_crime <- bind_rows(south_yorkshire, west_yorkshire)

# Step 6: Remove rows with missing Crime type or Location
cleaned_crime <- combined_crime %>%
  filter(!is.na(`Crime type`), !is.na(Location))

# ✅ Step 7: View unique districts (optional)
unique_districts <- cleaned_crime %>%
  distinct(County, District) %>%
  arrange(County, District)

print(unique_districts)

# ✅ Optional: If you want to group by district in your summary
crime_summary <- cleaned_crime %>%
  group_by(County, District, `Crime type`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(County, District, desc(Count))

# Step 8: View summarized crime data
print(crime_summary)

# Step 9: Save outputs
write_csv(cleaned_crime, "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_crime.csv")
write_csv(crime_summary, "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/crime_summary.csv")
write_csv(unique_districts, "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/unique_districts.csv")
