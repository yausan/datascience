# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load the crime summary dataset
crime_summary <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/crime_summary.csv")

# Filter only "Drugs" crime type
drug_offense_data <- crime_summary %>%
  filter(`Crime type` == "Drugs")

# Create filtered datasets for each county
south_drugs <- drug_offense_data %>%
  filter(County == "South Yorkshire")

west_drugs <- drug_offense_data %>%
  filter(County == "West Yorkshire")

# Plot 1: South Yorkshire
plot1 <- ggplot(south_drugs, aes(x = District, y = Count)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "South Yorkshire", x = "District", y = "Drug Offense Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: West Yorkshire
plot2 <- ggplot(west_drugs, aes(x = District, y = Count)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "West Yorkshire", x = "District", y = "Drug Offense Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display both plots side-by-side
grid.arrange(plot1, plot2, ncol = 2)
