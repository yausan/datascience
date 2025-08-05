library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the cleaned dataset
cleaned_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_house_prices.csv", show_col_types = FALSE)

# Convert numeric ymd format in 'Years' to Date, then extract Year
cleaned_data <- cleaned_data %>%
  mutate(
    Date_parsed = ymd(as.character(Years)),  # convert 20210715 to Date
    Year_only = year(Date_parsed)             # extract year like 2021
  )

# Filter for years 2021 to 2024
filtered_data <- cleaned_data %>%
  filter(Year_only %in% 2021:2024)

# Group by year, county, and district to get average prices
avg_price_by_district <- filtered_data %>%
  group_by(Year_only, County, District) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Plot line graph
ggplot(avg_price_by_district, aes(x = Year_only, y = Average_Price, color = District)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ County) +
  labs(
    title = "Average House Prices (2021–2024) by District in Each County",
    x = "Year",
    y = "Average Price (£)",
    color = "District"
  ) +
  theme_minimal()
