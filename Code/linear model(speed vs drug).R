library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Set working directory
setwd("/Users/karma/Coursework_yausan35D_230486")

# Load datasets
drug_rates <- read_csv("Cleaned_Data/drug_rates_2023.csv")
broadband_data <- read_csv("Cleaned_Data/cleanBroadband.csv")

# Clean county names
drug_rates <- drug_rates %>%
  mutate(county = str_trim(str_to_lower(county)))

broadband_data <- broadband_data %>%
  mutate(County = str_trim(str_to_lower(County)))

# Summarise broadband speeds for the two counties
broadband_summary <- broadband_data %>%
  filter(County %in% c("south yorkshire", "west yorkshire")) %>%
  group_by(County) %>%
  summarise(avg_download_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE))

# Join the data
comparison_df <- drug_rates %>%
  inner_join(broadband_summary, by = c("county" = "County"))




# Linear model
model <- lm(rate_per_10000 ~ avg_download_speed, data = comparison_df)
summary(model)

# Correlation
correlation <- cor(comparison_df$avg_download_speed, comparison_df$rate_per_10000)
cat("Correlation:", correlation)


ggplot(comparison_df, aes(x = avg_download_speed, y = rate_per_10000)) +
  geom_point(color = "blue", size = 4) +
  geom_text(aes(label = county), vjust = -1, size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Avg Download Speed vs Drug Offense Rate per 10,000 People",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offense Rate per 10,000"
  ) +
  theme_minimal()
