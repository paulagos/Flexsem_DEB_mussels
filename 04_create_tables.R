###############################################################################
# 04_create_tables.R
#
# Purpose:
# Master script for generating summary and comparison tables
# from DEB model outputs.
#
# Author: Paulo Lagos
###############################################################################

###############################################################################
# SECTION 1 - Load packages
###############################################################################

library(tidyverse)

###############################################################################
# SECTION 2 - Source helper functions
###############################################################################

#------------------------------------------------------------------------------
# Helper functions
#------------------------------------------------------------------------------
helper_files <- list.files(
  "R_scripts/functions/helpers",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(helper_files, source))

#------------------------------------------------------------------------------
# Metadata
#------------------------------------------------------------------------------
metadata_files <- list.files(
  "R_scripts/functions/metadata",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(metadata_files, source))

#------------------------------------------------------------------------------
# Summary functions
#------------------------------------------------------------------------------
summary_files <- list.files(
  "R_scripts/functions/summaries",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(summary_files, source))

#------------------------------------------------------------------------------
# Comparison functions
#------------------------------------------------------------------------------
comparison_files <- list.files(
  "R_scripts/functions/comparisons",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(comparison_files, source))

###############################################################################
# SECTION 3 - Load configuration
###############################################################################

source("R_scripts/01_get_config_DEBV2.R")

###############################################################################
# OUTPUT FOLDER
###############################################################################

table_run <- paste0(
  "Tables_",
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
)

table_output_dir <- file.path(
  master_folder,
  "Tables",
  table_run
)

dir.create(
  table_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

config <- list(
  
  master_folder = master_folder,
  
  table_output_dir = table_output_dir,
  
  emb_files = emb_files,
  
  layers = layers,
  
  elem = elem
  
)

###############################################################################
# SECTION 4 - Load DEB outputs
###############################################################################

rds_folder <- file.path(
  config$master_folder,
  "R_objects"
)

deb_data <- list(
  
  metadata = readRDS(
    file.path(rds_folder, "metadata.rds")
  ),
  
  deb_mussels = readRDS(
    file.path(rds_folder, "deb_mussels.rds")
  ),
  
  deb_fluxes = readRDS(
    file.path(rds_folder, "deb_fluxes.rds")
  ),
  
  deb_state_variables = readRDS(
    file.path(rds_folder, "deb_state_variables.rds")
  ),
  
  environment = readRDS(
    file.path(rds_folder, "environment.rds")
  ),
  
  mussels_farm = readRDS(
    file.path(rds_folder, "mussels_farm.rds")
  ),
  
  sediment = readRDS(
    file.path(rds_folder, "sediment.rds")
  )
  
)

###############################################################################
# SECTION 5 - Generate tables
###############################################################################

message("----------------------------------------")
message("Generating summary tables")
message("----------------------------------------")

###############################################################################
# CREATE OUTPUT FOLDERS
###############################################################################

summary_folder <- create_output_folder(
  config$table_output_dir,
  "Summaries"
)

comparison_folder <- create_output_folder(
  config$table_output_dir,
  "Comparisons"
)

###############################################################################
# SUMMARY TABLES
###############################################################################

locations <- get_locations(deb_data)

for (location in locations) {
  
  message("   Summary: ", location)
  
  table <- summary_location(
    deb_data = deb_data,
    location = location
  )
  
  save_DEB_table(
    table = table,
    filename = paste0("summary_", location, ".csv"),
    folder = summary_folder
  )
  
}

###############################################################################
# SECTION 6 - Finish
###############################################################################

message("----------------------------------------")
message("Finished generating tables.")
message("----------------------------------------")