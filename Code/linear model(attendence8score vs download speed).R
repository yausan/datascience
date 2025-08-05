library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load cleaned school data (already done)
cleaned_school_data_combined <- read_csv("Cleaned_Data/cleaned_school_data_combined.csv")

# Load broadband data
broadband_data <- read_csv("Cleaned_Data/cleanBroadband.csv")

# Prepare attainment summary for 2023 and relevant counties
attainment_summary <- cleaned_school_data_combined %>%
  mutate(county = str_to_lower(county)) %>%
  filter(year == 2023, county %in% c("south yorkshire", "west yorkshire")) %>%
  group_by(county) %>%
  summarise(mean_att8 = mean(att8scr, na.rm = TRUE))

# Prepare broadband summary for same counties
broadband_summary <- broadband_data %>%
  mutate(County = str_to_lower(County)) %>%
  filter(County %in% c("south yorkshire", "west yorkshire")) %>%
  group_by(County) %>%
  summarise(avg_download_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE))

# Merge data on county
comparison_df <- inner_join(attainment_summary, broadband_summary, by = c("county" = "County"))

# Print combined data
print(comparison_df)

# Calculate and print correlation
correlation <- cor(comparison_df$avg_download_speed, comparison_df$mean_att8)
cat("Correlation between Avg Download Speed and Attainment 8 Score:", round(correlation, 3), "\n")

# Linear regression model and summary
lm_model <- lm(mean_att8 ~ avg_download_speed, data = comparison_df)
cat("\nLinear Model Summary:\n")
print(summary(lm_model))

ggplot(comparison_df, aes(x = avg_download_speed, y = mean_att8)) +
  geom_point(color = "blue", size = 4) +
  geom_text(aes(label = str_to_title(county)), nudge_y = 0.5, size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # se=FALSE disables confidence band
  labs(
    title = "Average Download Speed vs Attainment 8 Score (2023)",
    x = "Average Download Speed (Mbit/s)",
    y = "Mean Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))
