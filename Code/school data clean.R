# Load necessary libraries
library(tidyverse)
library(readxl)
library(janitor)
library(purrr)

# Define paths
base_path <- "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/School_Data/"
output_folder <- "/Users/karma/Coursework_yausan35D_230486/Cleaned_Data/"

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Function to read and clean school data from a directory
process_school_data <- function(base_path) {
  # List all year folders
  year_folders <- c("Performancetables_21_22", "Performancetables_22_23", "Performancetables_23_24")
  
  # Create empty list to store all data
  all_data <- list()
  
  for (folder in year_folders) {
    folder_path <- file.path(base_path, folder)
    
    # Skip if folder doesn't exist
    if (!dir.exists(folder_path)) {
      message(paste("Folder not found:", folder_path))
      next
    }
    
    # List all files in the folder
    files <- list.files(folder_path, full.names = TRUE, pattern = "\\.(csv|xls|xlsx)$")
    
    if (length(files) == 0) {
      message(paste("No CSV or Excel files found in:", folder_path))
      next
    }
    
    for (file in files) {
      # Extract and format academic year from folder name
      academic_year <- str_extract(folder, "\\d{4}_\\d{4}") %>% 
        str_replace("(\\d{2})_(\\d{2})", "20\\1_20\\2")
      
      tryCatch({
        # Read file based on extension
        if (str_detect(file, "\\.csv$")) {
          df <- read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE) %>% 
            clean_names()
        } else if (str_detect(file, "\\.xls[x]?$")) {
          # Handle Excel files with potential unnamed columns
          df <- read_excel(file, .name_repair = "universal") %>% 
            clean_names() %>% 
            select(-matches("^x_|^\\.\\.\\."))  # Drop automatically named columns
        }
        
        # Add metadata
        df <- df %>% 
          mutate(
            source_file = basename(file),
            academic_year = academic_year,
            file_type = case_when(
              str_detect(file, "ks4final") ~ "ks4_final",
              str_detect(file, "ks4provisional") ~ "ks4_provisional",
              str_detect(file, "underlying") ~ "underlying_data",
              str_detect(file, "school_information") ~ "school_info",
              TRUE ~ "other"
            )
          )
        
        # Store in list
        all_data[[paste(academic_year, basename(file), sep = "_")]] <- df
        
      }, error = function(e) {
        message(paste("Error processing file:", file))
        message(e$message)
      })
    }
  }
  
  return(all_data)
}

# Enhanced function to clean individual data frames
clean_school_df <- function(df) {
  df %>% 
    # Convert special codes to NA
    mutate(across(everything(), ~ na_if(., "NP")),
           across(everything(), ~ na_if(., "NE")),
           across(everything(), ~ na_if(., "SUPP")),
           across(everything(), ~ na_if(., "NULL"))) %>% 
    # Convert percentage columns (handle % signs)
    mutate(across(matches("pt|percent|pct|p_|proportion|rate"), 
                  ~ case_when(
                    is.na(.) ~ NA_real_,
                    str_detect(., "%") ~ as.numeric(str_remove(., "%"))/100,
                    . == "NP" ~ NA_real_,
                    . == "NE" ~ NA_real_,
                    TRUE ~ as.numeric(.)
                  ))) %>%
    # Convert numeric columns
    mutate(across(any_of(c("attainment_8_score", "progress_8_score", 
                           "percentage", "score", "value", "average", 
                           "total", "count", "number", "amount", "sum")), 
                  ~ as.numeric(.))) %>%
    # Clean character columns
    mutate(across(where(is.character), ~ na_if(., ""))) %>% 
    mutate(across(where(is.character), str_trim)) %>% 
    # Standardize school names if present
    {if ("school_name" %in% names(.)) mutate(., school_name = str_to_title(school_name)) else .} %>% 
    # Convert date columns if present
    {if ("opendate" %in% names(.)) mutate(., opendate = as.Date(opendate, format = "%d/%m/%Y")) else .} %>% 
    {if ("closedate" %in% names(.)) mutate(., closedate = as.Date(closedate, format = "%d/%m/%Y")) else .}
}

# Main processing function
clean_all_school_data <- function(base_path) {
  # Read all raw data
  raw_data <- process_school_data(base_path)
  
  # Clean all data frames
  clean_data <- map(raw_data, clean_school_df)
  
  # Combine all data into one dataframe
  combined_data <- bind_rows(clean_data)
  
  return(combined_data)
}

# Execute the cleaning process
school_clean_data <- clean_all_school_data(base_path)

# Save cleaned data as single CSV file
write_csv(school_clean_data, file.path(output_folder, "school_clean_data.csv"))

# Print summary
message("\nCleaning complete! Combined data saved to:")
message(file.path(output_folder, "school_clean_data.csv"))
message("\nDataset dimensions: ", nrow(school_clean_data), " rows, ", ncol(school_clean_data), " columns")

# View the structure of your cleaned data
glimpse(school_clean_data)
