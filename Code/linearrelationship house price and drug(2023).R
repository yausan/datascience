library(readr)
library(dplyr)
library(ggplot2)

# Load cleaned datasets
drug_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/drug_rates_2023.csv")
house_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_house_prices.csv")

# Check column names in house_data to confirm 'County' exists
print(colnames(house_data))

# Create a lowercase version of County column as 'county' for standardization
house_data <- house_data %>%
  mutate(county = tolower(County))

# Check if all counties in drug_data are in house_data
missing_counties <- setdiff(unique(drug_data$county), unique(house_data$county))
if(length(missing_counties) > 0) {
  warning("Counties missing in house_data: ", paste(missing_counties, collapse = ", "))
} else {
  message("All counties in drug_data found in house_data.")
}

# Aggregate house prices per county
house_summary <- house_data %>%
  group_by(county) %>%
  summarise(
    avg_price = mean(Price, na.rm = TRUE),
    median_price = median(Price, na.rm = TRUE),
    n_transactions = n(),
    .groups = "drop"
  )

# Merge with drug_data on county
combined_data <- drug_data %>%
  inner_join(house_summary, by = "county")

# View combined data
print(combined_data)

# Plot scatterplot with points only (no regression smoothing due to small sample size)
ggplot(combined_data, aes(x = rate_per_10000, y = avg_price)) +
  geom_point(color = "blue", size = 4) +
  labs(
    title = "Average House Price vs Drug Crime Rate per County",
    x = "Drug Crime Rate (per 10,000 people)",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

# Optional: add a line connecting points (meaningful only for a few points)
ggplot(combined_data, aes(x = rate_per_10000, y = avg_price)) +
  geom_point(color = "blue", size = 4) +
  geom_line(color = "red") +
  labs(
    title = "Average House Price vs Drug Crime Rate per County",
    x = "Drug Crime Rate (per 10,000 people)",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

# Calculate correlation coefficient manually (no test if < 3 points)
if(nrow(combined_data) >= 3) {
  corr_test <- cor.test(combined_data$rate_per_10000, combined_data$avg_price)
  print(corr_test)
} else {
  corr <- cor(combined_data$rate_per_10000, combined_data$avg_price, use = "complete.obs")
  message("Correlation coefficient (no statistical test due to small sample size): ", corr)
}
