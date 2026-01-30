# Load packages
source("website_updates/load_packages.R")
gs4_deauth()

# --- 1. Read the Google Sheet ---
sheet_url <- "https://docs.google.com/spreadsheets/d/1qMqUIbDb5mQyUMyyG3EU-maD-oxepkA8-MSTzEtSQQo/edit?usp=sharing"
personnel_raw <- read_sheet(sheet_url)

# --- 2. Clean and prepare fields ---
personnel <- personnel_raw %>%
  mutate(
    id = str_to_lower(paste(first_name, last_name, sep = "_")),
    name = full_name,
    
    # Handle degree end year
    degree_end_year = as.character(degree_end_year),
    is_present = str_detect(str_to_lower(degree_end_year), "present"),
    degree_end_year_num = if_else(
      is_present,
      as.numeric(year(Sys.Date())),
      suppressWarnings(as.numeric(degree_end_year))
    ),
    
    image = file.path("/people/images/profile_pics", image_name),
    
    research = ifelse(
      is.na(project_description_abbreviated) | project_description_abbreviated == "",
      NA,
      paste0("**Research subject:** ", project_description_abbreviated)
    ),
    
    location = ifelse(
      is.na(project_location) | project_location == "",
      NA,
      paste0("**Research location:** ", project_location)
    ),
    
    raw_dates = dates,
    category = category
  ) %>%
  mutate(
    # --- Sorting logic ---
    sorting_code = case_when(
      
      # Current members
      category == "Current" & str_detect(status, regex("BS", ignore_case = TRUE)) ~ 10000 + degree_end_year_num,
      category == "Current" & str_detect(status, regex("MS", ignore_case = TRUE)) ~ 20000 + degree_end_year_num,
      category == "Current" & str_detect(status, regex("PhD", ignore_case = TRUE)) ~ 30000 + degree_end_year_num,
      category == "Current" & str_detect(status, regex("Principal Investigator", ignore_case = TRUE)) ~ 40000,
      
      # Alumni members
      category == "Alumni" & str_detect(status, regex("PhD", ignore_case = TRUE)) ~ 10000 - degree_end_year_num,
      category == "Alumni" & str_detect(status, regex("MS", ignore_case = TRUE)) ~ 20000 - degree_end_year_num,
      category == "Alumni" & str_detect(status, regex("BS", ignore_case = TRUE)) ~ 30000 - degree_end_year_num,
      
      # Fallback for any other case
      TRUE ~ 99999
    ),
    
    # Make Patrick Kelley always first
    sorting_code = if_else(full_name == "Patrick Kelley", 1L, sorting_code),
    
    # Display formatting
    name = paste0("**", full_name, "**<br>*", status, "*"),
    dates = if_else(
      full_name == "Patrick Kelley" | is.na(raw_dates) | raw_dates == "",
      NA_character_,
      paste0("**Dates:** ", raw_dates)
    ),
    location = if_else(full_name == "Patrick Kelley", NA_character_, location)
  ) %>%
  select(
    id, first_name, last_name, name, status, image,
    research, location, dates, category, sorting_code,
    degree_end_year_num
  )

# --- 5. Split dataset by category ---
current_members <- personnel %>% filter(category == "Current") %>% arrange(sorting_code)
alumni_members  <- personnel %>% filter(category == "Alumni") %>% arrange(sorting_code)

