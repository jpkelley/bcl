source("website_updates/load_packages.R")

# Disable authentication for public access
gs4_deauth()

# --- 1. Read the Google Sheet ---
sheet_url <- "https://docs.google.com/spreadsheets/d/1jJBmSC1oI6arrZsU9N4eDygrpN_awypSOk6ux13QcEk/edit?gid=0#gid=0"
personnel_raw <- read_sheet(sheet_url)


category_map <- c(
  datascience = "Project PROBE",
  contest = "contest assessment",
  deeptropics = "Deep Tropics Project",
  vine = "Hawaii VINE Project",
  plumas = "Panama PLUMAS Project",
  predators = "Project BITE",
  animalcomm = "Animal Communication & Contest Dynamics"
)

category_columns <- names(category_map)



# --- GENERATE QMD FILES ---
for(i in seq_len(nrow(personnel_raw))) {
  row <- personnel_raw[i, ]
  
  title <- gsub('"', "'", row$title)
  description_text <- gsub('"', "'", row$description)
  
  image_file <- row$image
  image_path <- paste0("../images/card_pics", "/", image_file)
  
  # Build categories list safely
  cats <- category_columns[!is.na(row[category_columns]) & row[category_columns] == 1]
  cat_names <- category_map[cats]
  
  # Format categories string for YAML
  if(length(cat_names) == 0) {
    categories_str <- "[]"
  } else if(length(cat_names) == 1) {
    categories_str <- paste0('["', cat_names, '"]')
  } else {
    quoted <- paste0('"', cat_names, '"')
    categories_str <- paste0("[", paste(quoted, collapse = ", "), "]")
  }
  
  # Include contributors and feature_order columns from the sheet
  contributors <- gsub('"', "'", row$contributors)
  feature_order <- gsub('"', "'", row$feature_order)
  
  # Prepare YAML header
  yaml_header <- paste0(
    "---\n",
    'title: "', title, '"\n',
    'image: "', image_path, '"\n',
    'description: "', description_text, '"\n',
    'categories: ', categories_str, '\n',
    'contributors: "', contributors, '"\n',      # <-- contributors
    'feature_order: "', feature_order, '"\n',    # <-- feature_order added
    "format:\n",
    "  html:\n",
    "    toc: FALSE\n",
    "---\n"
  )
  
  # Slugify title for filename
  filename <- gsub("[^A-Za-z0-9]+", "_", tolower(title))
  filename <- paste0(filename, ".qmd")
  
  output_folder <- "./research/research_profiles"
  filepath <- file.path(output_folder, filename)
  
  # Write QMD file
  writeLines(yaml_header, filepath, useBytes = TRUE)
}

cat("Generated", nrow(personnel_raw), "QMD files in", output_folder, "\n")
