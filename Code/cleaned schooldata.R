library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(purrr)

# --------------------- Define File Paths and Year Mapping ---------------------
folder_paths <- c(
  "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/School_Data/Performancetables_21_22",
  "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/School_Data/Performancetables_22_23",
  "/Users/karma/Coursework_yausan35D_230486/Obtained_Data/School_Data/Performancetables_23_24"
)

year_map <- c(
  "Performancetables_21_22" = 2022,
  "Performancetables_22_23" = 2023,
  "Performancetables_23_24" = 2024
)

district_codes <- c("370", "371", "372", "373",  # South Yorkshire
                    "380", "381", "382", "383", "384")  # West Yorkshire

# --------------------- Read and Clean All School CSV Data ---------------------
school_data_all <- map_dfr(folder_paths, function(path) {
  file <- file.path(path, "england_ks4final.csv")
  folder_name <- basename(path)
  year_value <- year_map[folder_name]
  
  if (file.exists(file)) {
    df <- read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      clean_names() %>%
      mutate(year = year_value)
    
    # Convert relevant columns to numeric if possible
    df <- df %>%
      mutate(
        att8scr = na_if(att8scr, "SUP"),
        att8scr = na_if(att8scr, "NE"),
        att8scr = as.numeric(att8scr),
        p8mea = na_if(p8mea, "SUP"),
        p8mea = na_if(p8mea, "NE"),
        p8mea = as.numeric(p8mea)
      )
    
    return(df)
  } else {
    warning(paste("Missing file in:", path))
    return(NULL)
  }
})

# Filter rows where lea is in district_codes
school_data_all <- school_data_all %>%
  filter(lea %in% district_codes)

# Optional: Check the class of lea
print(class(school_data_all$lea))

# Save cleaned and filtered data to CSV
write_csv(school_data_all, "cleaned_school_data.csv")


