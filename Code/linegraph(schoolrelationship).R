library(readr)
library(dplyr)
library(ggplot2)

# Step 1: Load cleaned school data
cleaned_school_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_school_data.csv")

# Step 2: Map LEA codes to real district names
lea_name_map <- tibble(
  lea = c("370", "371", "372", "373",  # South Yorkshire
          "380", "381", "382", "383", "384"),  # West Yorkshire
  district = c(
    "Barnsley", "Doncaster", "Rotherham", "Sheffield",     # South Yorkshire
    "Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield"  # West Yorkshire
  )
)

# Ensure LEA codes are numeric
lea_name_map <- lea_name_map %>%
  mutate(lea = as.numeric(lea))

cleaned_school_data <- cleaned_school_data %>%
  mutate(lea = as.numeric(lea))

# Step 3: Prepare data for plotting
plot_data <- cleaned_school_data %>%
  filter(lea %in% lea_name_map$lea) %>%
  left_join(lea_name_map, by = "lea") %>%
  group_by(year, district) %>%
  summarise(avg_att8 = mean(att8scr, na.rm = TRUE)) %>%
  ungroup()

# Step 4: Plot the line graph
ggplot(plot_data, aes(x = year, y = avg_att8, color = district)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Attainment 8 Scores Over Years by District (South & West Yorkshire)",
    x = "Year",
    y = "Average Attainment 8 Score",
    color = "District"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
