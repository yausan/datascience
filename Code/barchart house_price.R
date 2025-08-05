library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)  # for formatting numbers with commas

# Load the cleaned dataset
cleaned_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_house_prices.csv", show_col_types = FALSE)

# Convert 'Years' column from numeric ymd to Date and extract Year
cleaned_data <- cleaned_data %>%
  mutate(
    Date_parsed = ymd(as.character(Years)),
    Year_only = year(Date_parsed)
  )

# Filter for the year 2023
data_2023 <- cleaned_data %>%
  filter(Year_only == 2023)

# Calculate average prices grouped by County and District
avg_price_2023 <- data_2023 %>%
  group_by(County, District) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Plot bar chart with y-axis labels formatted with commas
ggplot(avg_price_2023, aes(x = reorder(District, Average_Price), y = Average_Price, fill = County)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +  # flip coordinates for better readability
  scale_y_continuous(labels = comma) +  # format y axis labels with commas
  labs(
    title = "Average House Prices in 2023 by District and County",
    x = "District",
    y = "Average Price (£)",
    fill = "County"
  ) +
  theme_minimal()
