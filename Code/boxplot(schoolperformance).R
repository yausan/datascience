library(tidyverse)

# 1. Load data focusing only on essential columns
school_data <- read_csv(
  "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/school_clean_data.csv",
  col_types = cols(
    urn = col_double(),
    lea = col_double(),
    laname = col_character(),
    att8scr = col_double()
  )
) %>% 
  select(urn, lea, laname, att8scr)

# 2. Directly filter for South Yorkshire schools using LEA codes
south_yorks_codes <- c(370, 371, 372, 373)
school_data_clean <- school_data %>%
  filter(lea %in% south_yorks_codes, !is.na(att8scr)) %>%
  mutate(
    district = case_when(
      lea == 370 ~ "Barnsley",
      lea == 371 ~ "Doncaster",
      lea == 372 ~ "Rotherham",
      lea == 373 ~ "Sheffield",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(district))

# 3. Create visualization
if(nrow(school_data_clean) > 0) {
  ggplot(school_data_clean, aes(x = district, y = att8scr, fill = district)) +
    geom_boxplot() +
    labs(title = "Attainment 8 Scores in South Yorkshire (2021/22)",
         subtitle = paste("Data from", nrow(school_data_clean), "schools"),
         x = "Local Authority District",
         y = "Attainment 8 Score") +
    theme_minimal() +
    theme(legend.position = "none")
} else {
  cat("No South Yorkshire schools found with attainment scores.")
}

# 4. Show summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
school_data_clean %>%
  group_by(district) %>%
  summarise(
    Schools = n(),
    Avg_Score = mean(att8scr, na.rm = TRUE),
    Median_Score = median(att8scr, na.rm = TRUE)
  ) %>%
  print()

ggplot(school_data_clean, aes(x = fct_reorder(district, att8scr, median), 
                              y = att8scr, 
                              fill = district)) +
  geom_boxplot(alpha = 0.8, width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Secondary School Performance in South Yorkshire (2021/22)",
       subtitle = paste("Attainment 8 Scores from", nrow(school_data_clean), "state-funded schools"),
       x = "Local Authority District",
       y = "Attainment 8 Score",
       caption = "Source: Department for Education\nNote: White diamonds show mean scores") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  coord_flip()
