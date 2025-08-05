library(tidyverse)

# Step 1: Read both broadband datasets
performance <- read_csv("/Users/karma/Coursework_yausan35D_230486/Obtained_Data/broadband/fixed_postcode_performance.csv")
coverage <- read_csv("/Users/karma/Coursework_yausan35D_230486/Obtained_Data/broadband/fixed_postcode_coverage.csv")

# Step 2: Standardize postcode column names
names(performance)[names(performance) == "postcode"] <- "Postcode"
names(coverage)[names(coverage) == "postcode"] <- "Postcode"

# Step 3: Merge datasets on Postcode
broadband_combined <- left_join(performance, coverage, by = "Postcode")

# Step 4: Filter out rows with missing or empty postcodes
broadband_combined <- broadband_combined %>%
  filter(!is.na(Postcode), Postcode != "")

# Step 5: Label counties by postcode prefixes
broadband_combined <- broadband_combined %>%
  mutate(
    County = case_when(
      str_starts(Postcode, "S")  ~ "South Yorkshire",
      str_starts(Postcode, "DN") ~ "South Yorkshire",
      str_starts(Postcode, "LS") ~ "West Yorkshire",
      str_starts(Postcode, "WF") ~ "West Yorkshire",
      str_starts(Postcode, "HD") ~ "West Yorkshire",
      str_starts(Postcode, "BD") ~ "West Yorkshire",
      str_starts(Postcode, "HX") ~ "West Yorkshire",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(County))

# Step 6: Create new column for connections under 10 Mbps
broadband_combined <- broadband_combined %>%
  mutate(
    Connections_under_10 = 
      `Number of connections < 2 Mbit/s (number of lines)` +
      `Number of connections 2<5 Mbit/s (number of lines)` +
      `Number of connections 5<10 Mbit/s (number of lines)`
  )

# Step 7: Select useful columns
broadband_filtered <- broadband_combined %>%
  select(
    Postcode, County,
    `Average download speed (Mbit/s)`,
    `Average upload speed (Mbit/s)`,
    `Median download speed (Mbit/s)`,
    `Median upload speed (Mbit/s)`,
    Connections_under_10,
    `% of premises unable to receive 2Mbit/s`,
    `% of premises unable to receive 5Mbit/s`,
    `% of premises unable to receive 10Mbit/s`,
    `% of premises unable to receive 30Mbit/s`,
    `FTTP availability (% premises)`
  )

# Step 8: Remove rows where all data (except postcode & county) is NA
broadband_filtered <- broadband_filtered %>%
  filter(rowSums(is.na(select(., -Postcode, -County))) < ncol(select(., -Postcode, -County)))

# Step 9: Remove columns that are entirely NA
broadband_clean <- broadband_filtered %>%
  select(where(~ !all(is.na(.))))

# Step 10: Save final cleaned dataset
write_csv(broadband_clean, "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleanBroadband.csv")

colSums(is.na(broadband_combined))               # Count of NAs
colSums(broadband_combined == "", na.rm = TRUE)  # Count of empty strings

