library(readr)
library(dplyr)
library(ggplot2)

# Load data
crime_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_crime.csv")

# Filter robbery data for a specific county and month
robbery_data <- crime_data %>%
  filter(
    County == "West Yorkshire",     # Change to "South Yorkshire" if needed
    `Crime type` == "Robbery",
    Month == "2025-05"
  )

# Summarize count of robbery by District
robbery_summary <- robbery_data %>%
  group_by(District) %>%
  summarise(Count = n()) %>%
  ungroup()

# Calculate percentage for labels
robbery_summary <- robbery_summary %>%
  mutate(Percent = round(Count / sum(Count) * 100, 1),
         Label = paste0(District, "\n", Percent, "%"))

# Plot pie chart
ggplot(robbery_summary, aes(x = "", y = Count, fill = District)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Robbery Rate by District - West Yorkshire (May 2025)",
       x = NULL, y = NULL, fill = "District") +
  theme_void() +
  theme(legend.position = "right")
