# Load required libraries
library(readr)
library(dplyr)
library(scales)   # for rescale()

# Step 1: Load datasets
house_prices <- read_csv("Cleaned_Data/house_price_summary.csv")
crime_ <- read_csv("Cleaned_Data/crime_summary.csv")
school <- read_csv("Cleaned_Data/school_summary.csv")
population <- read_csv("Cleaned_Data/population_summary.csv")

# Step 2: Clean District columns in all datasets (trim spaces and lowercase)
house_prices <- house_prices %>%
  mutate(District = tolower(trimws(District)))

crime_ <- crime_ %>%
  mutate(District = tolower(trimws(District)))

school <- school %>%
  mutate(District = tolower(trimws(District)))

population <- population %>%
  mutate(District = tolower(trimws(District)))

# Step 3: Aggregate crime counts by District (sum over all crime types)
crime_summary <- crime_ %>%
  group_by(District) %>%
  summarise(Count = sum(Count, na.rm = TRUE),
            .groups = "drop")

# Step 4: Merge datasets by District using inner_join
district_summary <- house_prices %>%
  inner_join(crime_summary, by = "District") %>%
  inner_join(school, by = "District") %>%
  inner_join(population, by = "District")

# Step 5: Check data before scoring
print(district_summary)

# Step 6: Normalize and score each characteristic (0-10 scale)
district_scores <- district_summary %>%
  mutate(
    score_house_price = rescale(avg_house_price, to = c(10, 0)),  # lower price is better
    score_crime = rescale(Count, to = c(10, 0)),                  # lower crime is better
    score_attainment = rescale(average_att8scr, to = c(0, 10)),   # higher attainment better
    score_population = rescale(total_population, to = c(0, 10))   # higher population better
  ) %>%
  # Step 7: Calculate composite score as mean of all scores
  mutate(
    composite_score = rowMeans(select(., starts_with("score_")))
  ) %>%
  arrange(desc(composite_score))

# Step 8: View final scored summary
print(district_scores)

# Save the merged district summary
write_csv(district_summary, "Cleaned_Data/district_combined_summary.csv")

