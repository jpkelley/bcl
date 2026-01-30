packages <- c(
  "googlesheets4", # for interfacing with Google Sheet in WYOBIRD shared folder
  "dplyr",         # for data wrangling
  "yaml",          # for conversion to YAML format (output)
  "stringr",
  "purrr",
  "glue",
  "fs",
  "tidyverse",
  "janitor"
  )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
