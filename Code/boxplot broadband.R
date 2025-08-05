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

# 2) Read & clean postcode→LSOA lookup
postcode_lsoa <- read_csv(
  "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/Postcode_to_LSOA_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(Postcode = toupper(str_trim(Postcode))) %>%
  select(Postcode, District, County)

# 3) Inner-join and build the four columns
merged_df <- broadband %>%
  inner_join(
    postcode_lsoa,
    by = c("Postcode_fixed" = "Postcode")
  ) %>%
  transmute(
    Postcode      = Postcode_fixed,
    County        = County.y,
    District      = District,
    Download_Mbps = Download_Mbps
  ) %>%
  # 4) Drop any NA or non-finite speeds
  filter(!is.na(Download_Mbps), is.finite(Download_Mbps)) %>%
  # 5) One row per postcode
  group_by(Postcode, County, District) %>%
  summarise(Download_Mbps = mean(Download_Mbps), .groups = "drop")

# 6) Split into the two counties
sy <- merged_df %>% filter(County == "South Yorkshire")
wy <- merged_df %>% filter(County == "West Yorkshire")

# 7) Boxplots by district
p_sy <- ggplot(sy, aes(x = District, y = Download_Mbps, fill = District)) +
  geom_boxplot() +
  labs(
    title = "Download Speed by District — South Yorkshire",
    x     = NULL,
    y     = "Avg Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

p_wy <- ggplot(wy, aes(x = District, y = Download_Mbps, fill = District)) +
  geom_boxplot() +
  labs(
    title = "Download Speed by District — West Yorkshire",
    x     = NULL,
    y     = "Avg Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# 8) Display side by side
(p_sy | p_wy) +
  plot_annotation(
    title   = "Broadband Download Speeds by District",
    caption = "Each box shows the distribution of postcode-level average download speeds"
  )
