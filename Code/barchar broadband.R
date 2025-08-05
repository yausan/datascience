# Load necessary libraries
library(tidyverse)
library(stringr)
library(patchwork)

# 1) Read & fix broadband data
broadband <- read_csv(
  "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleanBroadband.csv",
  show_col_types = FALSE
) %>%
  mutate(
    Postcode       = toupper(str_trim(Postcode)),
    Postcode_fixed = str_replace(Postcode, "(.+)(.{3})$", "\\1 \\2"),
    Download_Mbps  = `Average download speed (Mbit/s)`
  )

# 2) Read & clean postcode→LSOA lookup (include Town now)
postcode_lsoa <- read_csv(
  "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/Postcode_to_LSOA_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(Postcode = toupper(str_trim(Postcode))) %>%
  select(Postcode, Town, District, County)

# 3) Inner‑join so only matching postcodes remain; select four key cols
merged_df <- broadband %>%
  inner_join(
    postcode_lsoa,
    by = c("Postcode_fixed" = "Postcode")
  ) %>%
  transmute(
    Postcode      = Postcode_fixed,
    County        = County.y,
    District      = District,
    Town          = Town,
    Download_Mbps = Download_Mbps
  ) %>%
  filter(!is.na(Download_Mbps), is.finite(Download_Mbps)) %>%
  group_by(Postcode, County, District, Town) %>%
  summarise(Download_Mbps = mean(Download_Mbps), .groups = "drop")

# 4) Compute average by Town, split by County
avg_town <- merged_df %>%
  group_by(County, Town) %>%
  summarise(Avg_Speed = mean(Download_Mbps), .groups = "drop")

# 5) Separate datasets
sy_town <- avg_town %>% filter(County == "South Yorkshire")
wy_town <- avg_town %>% filter(County == "West Yorkshire")

# 6) Bar charts
p_bar_sy <- ggplot(sy_town, aes(x = reorder(Town, Avg_Speed), y = Avg_Speed, fill = Town)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Avg Download Speed by Town — South Yorkshire",
    x     = "Town",
    y     = "Avg Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p_bar_wy <- ggplot(wy_town, aes(x = reorder(Town, Avg_Speed), y = Avg_Speed, fill = Town)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Avg Download Speed by Town — West Yorkshire",
    x     = "Town",
    y     = "Avg Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 7) Display side by side
(p_bar_sy | p_bar_wy) +
  plot_annotation(
    title   = "Broadband Average Download Speed by Town",
    caption = "One bar per town, showing mean postcode‑level speeds"
  )
