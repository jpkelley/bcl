source("website_updates/load_packages.R")

# Disable authentication for public access
gs4_deauth()

# --- 1. Read the Google Sheet ---
sheet_url <- "https://docs.google.com/spreadsheets/d/1p_KhQlzuIJqhTq8mX3-pp9tcriJHsQSbb5MCPv3SBmQ/edit?usp=sharing"
personnel_raw <- read_sheet(sheet_url)

# --- 2. Clean and prepare fields ---
personnel <- personnel_raw %>%
  mutate(
    id = str_to_lower(paste(first_name, last_name, sep = "_")),
    name = full_name,
    degree_end_year = as.character(degree_end_year),
    
    # image path
    image = file.path("/images", "profile_pics", image_name),
    
    # research text
    research = ifelse(
      is.na(project_description_abbreviated) | project_description_abbreviated == "",
      NA,
      paste0("**Research subject:** ", project_description_abbreviated)
    ),
    
    # location text
    location = ifelse(
      is.na(project_location) | project_location == "",
      NA,
      paste0("**Research location:** ", project_location)
    ),
    
    # raw dates field preserved for QMD output
    raw_dates = dates,
    
    # formatted dates field
    dates = ifelse(
      is.na(raw_dates) | raw_dates == "",
      NA_character_,
      paste0("**Dates:** ", raw_dates)
    )
  ) %>%
  arrange(category_order) %>%   # <-- SORT BY CATEGORY_ORDER ONLY
  select(
    id, first_name, last_name, name, image,
    research, location, dates, category, category_order
  )

# --- 3. Function to create .qmd file for each person ---
create_person_qmd <- function(id, first_name, last_name, name, image,
                              research, location, dates, category, category_order) {
  
  # Determine output folder based on category
  folder <- ifelse(
    tolower(category) == "current",
    "listings/ind_profiles/current",
    "listings/ind_profiles/alumni"
  )
  
  file_name <- glue("{folder}/{first_name}_{last_name}.qmd")
  
  yaml_fields <- list(
    title = name,
    name = name,
    image = image,
    research = research,
    location = location,
    dates = dates,
    category = category,
    category_order = category_order,
    format = list(html = list(toc = FALSE))
  )
  
  # Exclude NA or empty fields
  yaml_fields <- yaml_fields[
    !sapply(yaml_fields, function(x) is.na(x) || x == "NA" || x == "")
  ]
  
  # Build YAML header
  yaml_header <- "---\n"
  for (field in names(yaml_fields)) {
    if (is.list(yaml_fields[[field]])) {
      yaml_header <- paste0(yaml_header, field, ":\n")
      for (sub in names(yaml_fields[[field]])) {
        yaml_header <- paste0(yaml_header, "  ", sub, ":\n")
        for (subsub in names(yaml_fields[[field]][[sub]])) {
          yaml_header <- paste0(
            yaml_header,
            "    ", subsub, ": ", yaml_fields[[field]][[sub]][[subsub]], "\n"
          )
        }
      }
    } else {
      yaml_header <- paste0(
        yaml_header,
        field, ": \"", yaml_fields[[field]], "\"\n"
      )
    }
  }
  yaml_header <- paste0(yaml_header, "---\n")
  
  write_lines(yaml_header, file_name)
}

# --- 4. Generate .qmd files ---
pwalk(personnel, create_person_qmd)
