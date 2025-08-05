library(tidyverse)

# 1. Load and prepare data
school_data <- read_csv(
  "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/school_clean_data.csv",
  col_types = cols(
    urn = col_double(),
    lea = col_double(),
    laname = col_character(),
    att8scr = col_double()
  )
)

# 2. Filter for West Yorkshire districts (official LEA codes)
west_yorks_codes <- c(
  380,  # Bradford
  381,  # Calderdale
  382,  # Kirklees
  383,  # Leeds
  384   # Wakefield
)

school_data_clean <- school_data %>%
  filter(lea %in% west_yorks_codes, !is.na(att8scr)) %>%
  mutate(
    district = case_when(
      lea == 380 ~ "Bradford",
      lea == 381 ~ "Calderdale",
      lea == 382 ~ "Kirklees",
      lea == 383 ~ "Leeds",
      lea == 384 ~ "Wakefield",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(district))

# 3. Create boxplot visualization
ggplot(school_data_clean, aes(x = fct_reorder(district, att8scr, median), 
                              y = att8scr, 
                              fill = district)) +
  geom_boxplot(alpha = 0.8, width = 0.6, outlier.shape = 8, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Secondary School Performance in West Yorkshire (2021/22)",
       subtitle = "Distribution of Attainment 8 Scores by Local Authority",
       x = "Local Authority District",
       y = "Attainment 8 Score",
       caption = "Source: Department for Education\nWhite diamonds show mean scores") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13)
  ) +
  coord_flip()

