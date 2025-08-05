# Load required libraries
library(dplyr)
library(readr)
library(scales)

# Define counties of interest
counties_of_interest <- c("south yorkshire", "west yorkshire")

# Load datasets
house_prices <- read_csv("Cleaned_Data/house_price_summary.csv")
broadband <- read_csv("Cleaned_Data/broadband_summary_join_with_postcode_to_lsoa.csv")
crime <- read_csv("Cleaned_Data/crime_summary.csv")
school <- read_csv("Cleaned_Data/school_summary.csv")
population <- read_csv("Cleaned_Data/population_summary.csv")

# Preprocess and filter each dataset to counties of interest
house_prices <- house_prices %>%
  mutate(District = tolower(trimws(District)),
         County = tolower(trimws(County))) %>%
  filter(County %in% counties_of_interest) %>%
  group_by(District) %>%
  summarise(avg_house_price = mean(avg_house_price, na.rm = TRUE), .groups = 'drop')

broadband <- broadband %>%
  mutate(District = tolower(trimws(District)),
         County = tolower(trimws(County))) %>%
  filter(County %in% counties_of_interest) %>%
  group_by(District) %>%
  summarise(avg_broadband_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = 'drop')

crime_total <- crime %>%
  mutate(District = tolower(trimws(District)),
         County = tolower(trimws(County))) %>%
  filter(County %in% counties_of_interest) %>%
  group_by(District) %>%
  summarise(total_crime_count = sum(Count, na.rm = TRUE), .groups = 'drop')

school_avg <- school %>%
  mutate(District = tolower(trimws(District)),
         county = tolower(trimws(county))) %>%
  filter(county %in% counties_of_interest) %>%
  group_by(District) %>%
  summarise(average_att8scr = mean(average_att8scr, na.rm = TRUE), .groups = 'drop')

population_sum <- population %>%
  mutate(District = tolower(trimws(District)),
         County = tolower(trimws(County))) %>%
  filter(County %in% counties_of_interest) %>%
  group_by(District) %>%
  summarise(total_population = sum(total_population, na.rm = TRUE), .groups = 'drop')

# Join all datasets by District
combined_data <- house_prices %>%
  inner_join(broadband, by = "District") %>%
  inner_join(crime_total, by = "District") %>%
  inner_join(school_avg, by = "District") %>%
  inner_join(population_sum, by = "District")

# Calculate crime rate per 10,000 people
combined_data <- combined_data %>%
  mutate(crime_rate_per_10000 = (total_crime_count / total_population) * 10000)

# Safe rescale function
safe_rescale <- function(x, to = c(0, 10)) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  if (min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
    return(rep(mean(to), length(x)))
  }
  rescale(x, to = to)
}

# Calculate normalized scores
scores_check <- combined_data %>%
  mutate(
    score_house_price = safe_rescale(avg_house_price, to = c(10, 0)),    # lower price better → reverse scale
    score_broadband   = safe_rescale(avg_broadband_speed, to = c(0, 10)), # higher speed better
    score_crime       = safe_rescale(crime_rate_per_10000, to = c(10, 0)), # lower crime better → reverse scale
    score_school      = safe_rescale(average_att8scr, to = c(0, 10))       # higher school score better
  )

# Check for NAs or NaNs in scores
na_summary <- scores_check %>%
  summarise(
    na_house_price = sum(is.na(score_house_price) | is.nan(score_house_price)),
    na_broadband   = sum(is.na(score_broadband) | is.nan(score_broadband)),
    na_crime       = sum(is.na(score_crime) | is.nan(score_crime)),
    na_school      = sum(is.na(score_school) | is.nan(score_school))
  )
print(na_summary)

# Optional: print problematic rows
problem_rows <- scores_check %>%
  filter(
    is.na(score_house_price) | is.nan(score_house_price) |
      is.na(score_broadband) | is.nan(score_broadband) |
      is.na(score_crime) | is.nan(score_crime) |
      is.na(score_school) | is.nan(score_school)
  )
if (nrow(problem_rows) > 0) {
  print(problem_rows)
} else {
  message("No rows with NA or NaN scores found.")
}

# Compute composite score and get top 5 districts
combined_scores <- scores_check %>%
  mutate(
    composite_score = round(rowMeans(select(., starts_with("score_")), na.rm = TRUE), 2)
  ) %>%
  arrange(desc(composite_score))

# Select top 5 districts without County column
top_5_districts <- combined_scores %>%
  select(District, composite_score, score_house_price, score_broadband, score_crime, score_school) %>%
  slice_head(n = 5)

# Print top 5 recommended districts
print(top_5_districts)

# Save to CSV file
write_csv(top_5_districts, "recommended_districts.csv")
