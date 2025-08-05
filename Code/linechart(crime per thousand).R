library(tidyverse)
library(stringr)

# Load datasets
Population <- read_csv("/Users/karma/Coursework_yausan35D_230486/Obtained_Data/population/Population.csv")
postcode_to_lsoa <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/Postcode_to_LSOA_clean.csv") %>%
  select(Postcode, LSOA_Code, District)
crime_data <- read_csv("/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/cleaned_crime.csv")

# Step 1: Clean whitespaces and make uppercase
Population <- Population %>%
  mutate(Postcode = str_replace_all(Postcode, "\\s+", "") %>% str_to_upper())

postcode_to_lsoa <- postcode_to_lsoa %>%
  mutate(Postcode = str_replace_all(Postcode, "\\s+", "") %>% str_to_upper())

# Step 2: Extract outward code from postcode_to_lsoa$Postcode
postcode_to_lsoa <- postcode_to_lsoa %>%
  mutate(Outward = str_extract(Postcode, "^[A-Z]{1,2}[0-9][A-Z0-9]?"))

# Step 3: Add outward code to Population (which is already outward)
Population <- Population %>%
  rename(Outward = Postcode)

# Step 4: Join on Outward code
district_population <- Population %>%
  left_join(postcode_to_lsoa, by = "Outward") %>%
  drop_na(District)

# Step 5: Summarise total population by District
district_population_summary <- district_population %>%
  group_by(District) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE)) %>%
  arrange(desc(Total_Population))

# Step 6: View the result
print(head(district_population_summary, 10))
# Step A: Collapse postcode_to_lsoa to one row per Outward code with majority or first matching District
postcode_outward_summary <- postcode_to_lsoa %>%
  group_by(Outward) %>%
  summarise(District = first(District))  # or use mode/majority if needed

# Step B: Join with Population (8035 rows) — now a 1:1 join
district_population <- Population %>%
  left_join(postcode_outward_summary, by = "Outward") %>%
  drop_na(District)

# Step C: Summarise total population by District
district_population_summary <- district_population %>%
  group_by(District) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE)) %>%
  arrange(desc(Total_Population))

cleaned_crime_data <- crime_data %>%
  filter(!is.na(District))
crime_summary <- cleaned_crime_data %>%
  group_by(District) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes))

head(crime_summary, 10)

crime_per_capita <- crime_summary %>%
  left_join(district_population_summary, by = "District")

crime_per_capita <- crime_per_capita %>%
  mutate(Crimes_per_1000 = (Total_Crimes / Total_Population) * 1000)

crime_per_capita %>%
  arrange(desc(Crimes_per_1000)) %>%
  select(District, Total_Crimes, Total_Population, Crimes_per_1000) %>%
  head(10)

library(ggplot2)

# Remove rows with NA values
crime_plot_data <- crime_per_capita %>%
  filter(!is.na(Crimes_per_1000))

# Plot
ggplot(crime_plot_data, aes(x = reorder(District, Crimes_per_1000), y = Crimes_per_1000)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime Rate per 1,000 People by District",
       x = "District",
       y = "Crimes per 1,000 People") +
  theme_minimal()











