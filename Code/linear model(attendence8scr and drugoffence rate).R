# Load required libraries
library(readr)
library(dplyr)
library(janitor)
library(purrr)
library(stringr)
library(ggplot2)

# Define folder paths and year mapping
folder_paths <- c(
  "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/School_Data/Performancetables_21_22",
  "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/School_Data/Performancetables_22_23",
  "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/School_Data/Performancetables_23_24"
)

year_map <- c(
  "Performancetables_21_22" = 2022,
  "Performancetables_22_23" = 2023,
  "Performancetables_23_24" = 2024
)

# Helper to auto-detect location columns
detect_location_cols <- function(df) {
  cols <- colnames(df)
  list(
    county = cols[str_detect(cols, regex("county|local_authority|la_name|lea", ignore_case = TRUE))][1],
    town = cols[str_detect(cols, regex("town|city|locality", ignore_case = TRUE))][1],
    district = cols[str_detect(cols, regex("district|borough|area", ignore_case = TRUE))][1]
  )
}

# Load and clean a single CSV
load_and_clean <- function(path) {
  file <- file.path(path, "england_ks4final.csv")
  folder_name <- basename(path)
  year_value <- year_map[folder_name]
  
  if (!file.exists(file)) {
    warning(paste("Missing:", file))
    return(NULL)
  }
  
  df <- read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE) %>% clean_names()
  loc_cols <- detect_location_cols(df)
  
  selected <- c("att8scr", loc_cols$county, loc_cols$district, loc_cols$town, "lea", "schname", "urn")
  selected <- selected[!is.na(selected)]
  
  df_sel <- df %>%
    mutate(
      att8scr = na_if(att8scr, "SUP"),
      att8scr = na_if(att8scr, "NE"),
      att8scr = as.numeric(att8scr)
    ) %>%
    select(all_of(selected)) %>%
    mutate(year = year_value) %>%
    filter(!is.na(att8scr))
  
  rename_map <- c()
  if (!is.na(loc_cols$county)) rename_map[loc_cols$county] <- "county"
  if (!is.na(loc_cols$district)) rename_map[loc_cols$district] <- "district"
  if (!is.na(loc_cols$town)) rename_map[loc_cols$town] <- "town"
  rename_map["schname"] <- "estabname"
  
  df_sel <- df_sel %>% rename(any_of(rename_map))
  return(df_sel)
}

# Load and combine all data
combined_data <- map_dfr(folder_paths, load_and_clean)

# Filter only South and West Yorkshire using LEA codes
south_yorkshire_leas <- c("370", "371", "372", "373")
west_yorkshire_leas <- c("380", "381", "382", "383", "384")

combined_data <- combined_data %>%
  filter(lea %in% c(south_yorkshire_leas, west_yorkshire_leas)) %>%
  mutate(county = case_when(
    lea %in% south_yorkshire_leas ~ "South Yorkshire",
    lea %in% west_yorkshire_leas ~ "West Yorkshire"
  ))

# Save cleaned dataset
write_csv(
  combined_data,
  "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_school_data_combined.csv"
)

# Extract 2023 only for analysis
attainment_2023 <- combined_data %>% filter(year == 2023)

# Average Attainment 8 by county
attainment_summary <- attainment_2023 %>%
  group_by(county) %>%
  summarise(mean_att8 = mean(att8scr, na.rm = TRUE), .groups = "drop")

# Drug crime rates (2023)
drug_data <- tibble(
  county = c("South Yorkshire", "West Yorkshire"),
  drug_crime_rate_per_10000 = c(33.11, 38.84),
  year = 2023
)

# Merge for analysis
combined_analysis <- inner_join(attainment_summary, drug_data, by = "county")

# Plot
ggplot(combined_analysis, aes(x = drug_crime_rate_per_10000, y = mean_att8, label = county)) +
  geom_point(size = 4, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text(nudge_y = 1.5) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate (2023)",
    x = "Drug Offense Rate per 10,000",
    y = "Mean Attainment 8 Score"
  ) +
  theme_minimal()

# Correlation and Linear Model
correlation <- cor(combined_analysis$mean_att8, combined_analysis$drug_crime_rate_per_10000)
cat("Correlation:", round(correlation, 3), "\n")

lm_model <- lm(mean_att8 ~ drug_crime_rate_per_10000, data = combined_analysis)
print(summary(lm_model))

# Final combined table
print(combined_analysis)
