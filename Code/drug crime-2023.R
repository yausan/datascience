library(readr)
library(dplyr)

# Set your folder path
data_path <- "/Users/karma/Coursework_yausan35D_230486/2023 crime data/"

# Load all CSVs recursively
csv_files <- list.files(path = data_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Read and extract relevant columns
crime_list <- lapply(csv_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  if (all(c("Reported by", "Crime type") %in% colnames(df))) {
    df %>% select(`Reported by`, `Crime type`)
  } else {
    NULL
  }
})

# Remove nulls and combine data
combined_crime <- bind_rows(crime_list[!sapply(crime_list, is.null)])

# Filter only drug crimes and clean names
drug_data <- combined_crime %>%
  filter(!is.na(`Reported by`), !is.na(`Crime type`)) %>%
  rename(county = `Reported by`, crimetype = `Crime type`) %>%
  mutate(
    county = trimws(tolower(county)),
    crimetype = trimws(tolower(crimetype))
  ) %>%
  filter(crimetype == "drugs")

# Summarize drug crime count by county
summary_drug_crime <- drug_data %>%
  group_by(county) %>%
  summarise(drug_crimes_2023 = n()) %>%
  filter(county %in% c("south yorkshire", "west yorkshire"))

# Save ready-to-use data
write_csv(summary_drug_crime, "/Users/karma/Coursework_yausan35D_230486/2023_drug_crime_summary.csv")

# Show final output
print(summary_drug_crime)

drug_summary <- drug_crime %>%
  group_by(county) %>%
  summarise(drug_crime_count = n())
View(drug_summary)

# Your current data
drug_summary <- data.frame(
  county = c("south yorkshire police", "west yorkshire police"),
  drug_crime_count = c(4689, 9065)
)

# Clean county names to match other data sources
drug_summary <- drug_summary %>%
  mutate(county = gsub(" police", "", tolower(county)))

# Add 2023 population (ONS mid-year estimates, approx)
population_data <- data.frame(
  county = c("south yorkshire", "west yorkshire"),
  population = c(1416000, 2334000)  # update if needed
)

# Join and compute drug rate per 10,000
drug_summary <- drug_summary %>%
  left_join(population_data, by = "county") %>%
  mutate(rate_per_10000 = round((drug_crime_count / population) * 10000, 2))

# Save the cleaned data
write_csv(drug_summary, "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/drug_rates_2023.csv")

# View final table
print(drug_summary)

