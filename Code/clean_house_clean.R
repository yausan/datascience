library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# File paths
files <- list(
  houseprice2021 = "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/House_Prices/houseprice2021.csv",
  houseprice2022 = "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/House_Prices/houseprice2022.csv",
  houseprice2023 = "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/House_Prices/houseprice2023.csv",
  houseprice2024 = "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/House_Prices/houseprice2024.csv"
)

# Column names
correct_names <- c("Transaction_ID", "Price", "Date", "Postcode", "Property_Type", 
                   "New_Build_Flag", "Tenure", "PAON", "SAON", "Street", "Locality", 
                   "Town_City", "District", "County", "Extra1", "Extra2")

# Function to read and clean a file with format-specific parsing
read_and_clean <- function(filepath, year_hint) {
  df <- read_csv(filepath, col_names = FALSE, show_col_types = FALSE)
  colnames(df) <- correct_names
  
  df <- df %>%
    mutate(
      Postcode = str_trim(toupper(Postcode)),
      County = str_trim(str_to_title(County)),
      # Apply date parsing depending on the year
      Years = case_when(
        year_hint == 2021 ~ as.character(mdy_hm(Date)),  # Special handling for 2021
        TRUE ~ as.character(parse_date_time(Date, orders = c("ymd HMS", "mdy HMS", "ymd", "mdy")))
      )
    ) %>%
    filter(!(is.na(Price) & is.na(Years) & is.na(Postcode) & is.na(County))) %>%
    select(-Extra1, -Extra2, -Date)  # Remove original 'Date', keep 'Years'
  
  return(df)
}

# Clean all files with correct date parsing logic
houseprice2021 <- read_and_clean(files$houseprice2021, 2021)
houseprice2022 <- read_and_clean(files$houseprice2022, 2022)
houseprice2023 <- read_and_clean(files$houseprice2023, 2023)
houseprice2024 <- read_and_clean(files$houseprice2024, 2024)

# Combine
combined_data <- bind_rows(houseprice2021, houseprice2022, houseprice2023, houseprice2024)

# Filter counties
combined_data <- combined_data %>%
  filter(County %in% c("West Yorkshire", "South Yorkshire")) %>%
  arrange(desc(Price))

# Save to cleaned folder
output_folder <- "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data"
if (!dir.exists(output_folder)) dir.create(output_folder)

write_csv(combined_data, file.path(output_folder, "cleaned_house_prices.csv"))

# Year Summary Table from 'Years' column
year_summary <- combined_data %>%
  mutate(Year = year(ymd_hms(Years))) %>%
  count(Year)

print(year_summary)
