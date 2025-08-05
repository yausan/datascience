# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)

# Step 1: Load cleaned datasets
house_prices <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_house_prices.csv")
broadband <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleanBroadband.csv")

# Step 2: Standardize Postcodes (remove spaces, make uppercase)
house_prices <- house_prices %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode)))

broadband <- broadband %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode)))

# Step 3: Group by Postcode and County, and calculate averages
avg_price_postcode <- house_prices %>%
  group_by(Postcode, County) %>%
  summarise(Avg_House_Price = mean(Price, na.rm = TRUE), .groups = "drop")

avg_speed_postcode <- broadband %>%
  group_by(Postcode, County) %>%
  summarise(Avg_Download_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = "drop")

# Step 4: Merge using inner_join on Postcode and County
merged_data <- inner_join(avg_price_postcode, avg_speed_postcode, by = c("Postcode", "County"))

# Optional: Filter to South Yorkshire and West Yorkshire only
merged_filtered <- merged_data %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire"))

# Step 5: Plot Linear Relationship
ggplot(merged_filtered, aes(x = Avg_Download_Speed, y = Avg_House_Price, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Linear Relationship Between House Price and Download Speed",
    x = "Average Download Speed (Mbit/s)",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

#  Linear Model Summary
model <- lm(Avg_House_Price ~ Avg_Download_Speed, data = merged_filtered)
summary(model)

# Correlation
correlation <- cor(merged_filtered$Avg_House_Price, merged_filtered$Avg_Download_Speed, use = "complete.obs")
print(paste("Correlation between Avg House Price and Download Speed:", round(correlation, 3)))
