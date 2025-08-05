

# Load libraries
library(readr)
library(dplyr)
library(lubridate)
library(fmsb)
crime_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_crime.csv")
vehicle_crime_may2025 <- crime_data %>%
  filter(County == "South Yorkshire",
         `Crime type` == "Vehicle crime",
         Month == "2025-05")
district_summary <- vehicle_crime_may2025 %>%
  group_by(District) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
# Handle radar chart input format
max_val <- max(district_summary$Count) + 5
min_val <- 0

radar_data <- rbind(
  rep(max_val, nrow(district_summary)),
  rep(min_val, nrow(district_summary)),
  district_summary$Count
)

colnames(radar_data) <- district_summary$District
rownames(radar_data) <- c("Max", "Min", "Actual")
radarchart(as.data.frame(radar_data),
           axistype = 1,
           pcol = "darkred",
           pfcol = rgb(1, 0.3, 0.3, 0.4),
           plwd = 2,
           cglcol = "gray", cglty = 1, cglwd = 0.8,
           axislabcol = "black", vlcex = 0.8,
           title = "Vehicle Crime by District - South Yorkshire (May 2025)")
