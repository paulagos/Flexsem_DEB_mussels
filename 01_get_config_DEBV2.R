################################################################################
# FLEXSEM DEB MUSSEL MODEL
# Script 1 - Configuration and Model Output Selection
#
# Description:
# This script defines all user settings required for reading FlexSem DEB mussel
# model outputs. It automatically locates the correct model output files and
# prepares the information required by the data-reading script.
#
# User configurable settings:
#   • Master output folder
#   • Results folder selection mode
#   • Extraction element
#   • Extraction layer
#   • Variables to read
#
# Outputs:
#   emb_files   -> Full paths to all model output files
#   variables   -> Variables to extract from the model
#   elem        -> Model element to extract
#   layer       -> Vertical layer for each FlexSem output
#
# Author: Paulo F. Lagos
# Created: 23 June 2026
# Last modified: 23 June 2026
################################################################################


# ==============================================================================
# LOAD REQUIRED PACKAGES
# ==============================================================================

source("C:/AU/R/emfunc.r")

library(FlexsemTools)


# ==============================================================================
# USER SETTINGS
# ==============================================================================

# Master folder containing all DEB model outputs

master_folder <- "C:/AU/farmsIDWNS2026/IDW/2019/DEB_outputs"


# ------------------------------------------------------------------------------
# RESULTS FOLDER SELECTION
#
# latest      -> newest results folder
# name        -> use a specific results folder
# interactive -> choose manually for each location
# ------------------------------------------------------------------------------

results_mode <- "latest"

# Used only when results_mode = "name"

results_folder_name <- "results_july_06"


#------------------------------------------------------------------------------
# MODEL EXTRACTION SETTINGS
# ------------------------------------------------------------------------------

# Model element to extract
elem <-191

# Vertical layer for each FlexSem output
#
# Most outputs are stored in the water column (layer 5).
# Sediment outputs are stored on a single layer (layer 0).

layers <- list(
  
  deb_mussels         = 5,
  deb_fluxes          = 5,
  deb_state_variables = 5,
  environment         = 5,
  mussels_farm        = 5,
  sediment            = 0
  
)

# ------------------------------------------------------------------------------
# MODEL OUTPUT FILES
#
# These FlexSem output files will be searched for inside each selected
# results folder. Variables will be discovered automatically later when
# the files are read.
# ------------------------------------------------------------------------------

output_files <- c(
  
  "deb_mussels.emb",
  
  "deb_fluxes.emb",
  
  "deb_state_variables.emb",
  
  "environment.emb",
  
  "mussels_farm.emb",
  
  "sediment.emb"
  
)


# ==============================================================================
# FIND MODEL LOCATION FOLDERS
# ==============================================================================

location_folders <- list.dirs(
  
  path = master_folder,
  
  recursive = FALSE,
  
  full.names = TRUE
  
)

location_folders <- location_folders[
  grepl("^Output_DEB_", basename(location_folders))
]


cat("\n")
cat("=========================================================\n")
cat(" FLEXSEM DEB MUSSEL MODEL OUTPUTS\n")
cat("=========================================================\n\n")

cat(length(location_folders),
    "model location(s) detected.\n\n")

###############################################################################
# INITIALIZE OUTPUT OBJECTS
###############################################################################

# One list element for each FlexSem output file
emb_files <- vector("list", length(output_files))

names(emb_files) <-
  tools::file_path_sans_ext(output_files)

# Track which locations were successfully processed
loaded_locations <- character()

missing_locations <- character()

selected_results_folders <- character()

selected_results_paths <- character()

# ==============================================================================
# FIND MODEL OUTPUT FILES
# ==============================================================================
for (loc in location_folders) {
  
  location_name <- sub("^Output_DEB_", "", basename(loc))
  
  
  results_folders <- list.dirs(
    
    path = loc,
    
    recursive = FALSE,
    
    full.names = TRUE
    
  )
  
  
  results_folders <- results_folders[
    grepl("^results", basename(results_folders))
  ]
  
  
  if (length(results_folders) == 0) {
    
    warning(
      "No results folders found for ",
      location_name
    )
    
    next
    
  }
  
  # --------------------------------------------------------------------------
  # Select results folder
  # --------------------------------------------------------------------------
  
  if (results_mode == "latest") {
    
    info <- file.info(results_folders)
    
    selected_folder <- results_folders[
      which.max(info$mtime)
    ]
    
  } else if (results_mode == "name") {
    
    selected_folder <- file.path(
      loc,
      results_folder_name
    )
    
    if (!dir.exists(selected_folder)) {
      
      warning(
        "Folder '",
        results_folder_name,
        "' not found for ",
        location_name
      )
      next
    }
    
  }else if (results_mode == "interactive") {
    
    cat("\n")
    cat("-----------------------------------------\n")
    cat("Location :", location_name, "\n")
    cat("-----------------------------------------\n")
    
    selection <- menu(
      
      basename(results_folders),
      
      title = "Select results folder"
      
    )
    
    if (selection == 0) {
      
      warning(
        "No folder selected for ",
        location_name
      )
      
      next
      
    }
    
    selected_folder <- results_folders[selection]
    
    
  }else{
    
    stop(
      "Unknown results_mode.\n",
      "Choose one of: latest, name or interactive."
    )
    
  }
  
  # --------------------------------------------------------------------------
  # Search for all requested FlexSem output files
  # --------------------------------------------------------------------------
  
  all_files_found <- TRUE
  
  for (file in output_files) {
    
    emb_file <- file.path(
      selected_folder,
      file
    )
    
    if (file.exists(emb_file)) {
      
      output_name <- tools::file_path_sans_ext(file)
      
      emb_files[[output_name]] <- c(
        emb_files[[output_name]],
        emb_file
      )
      
    } else {
      
      all_files_found <- FALSE
      
      cat(
        sprintf(
          "   Missing: %s\n",
          file
        )
      )
      
    }
    
  }
  
  # --------------------------------------------------------------------------
  # Record location status
  # --------------------------------------------------------------------------
  
  if (all_files_found) {
    
    loaded_locations <- c(
      loaded_locations,
      location_name
    )
    
    selected_results_folders <- c(
      selected_results_folders,
      basename(selected_folder)
    )
    
    selected_results_paths <- c(
      selected_results_paths,
      selected_folder
    )
    
    cat(
      sprintf(
        "%-18s -> %s\n",
        location_name,
        basename(selected_folder)
      )
    )
    
  } else {
    
    missing_locations <- c(
      missing_locations,
      location_name
    )
    
  } 
  
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("=========================================================\n")
cat("CONFIGURATION SUMMARY\n")
cat("=========================================================\n\n")

# --------------------------------------------------------------------------
# Location summary
# --------------------------------------------------------------------------

cat(sprintf("Locations detected      : %d\n", length(location_folders)))
cat(sprintf("Locations with outputs  : %d\n", length(loaded_locations)))
cat(sprintf("Locations pending       : %d\n\n", length(missing_locations)))

# --------------------------------------------------------------------------
# Location status
# --------------------------------------------------------------------------

cat("Location status:\n\n")

for (loc in location_folders) {
  
  location_name <- sub("^Output_DEB_", "", basename(loc))
  
  if (location_name %in% loaded_locations) {
    
    cat("  ✓", location_name, "\n")
    
  } else {
    
    cat("  ○", location_name, "(no results available)\n")
    
  }
  
}

cat("\n")

# --------------------------------------------------------------------------
# Output file inventory
# --------------------------------------------------------------------------

cat("FlexSem output files found:\n\n")

for (name in names(emb_files)) {
  
  cat(
    sprintf(
      "  %-22s : %3d file(s)\n",
      name,
      length(emb_files[[name]])
    )
  )
  
}

cat("\n")

# --------------------------------------------------------------------------
# Check for missing output files
# --------------------------------------------------------------------------

expected_files <- length(loaded_locations)

missing_found <- FALSE

for (name in names(emb_files)) {
  
  n_found <- length(emb_files[[name]])
  
  if (n_found < expected_files) {
    
    missing_found <- TRUE
    
    cat(
      sprintf(
        "WARNING: %s is missing %d output file(s).\n",
        name,
        expected_files - n_found
      )
    )
    
  }
  
}

if (!missing_found) {
  
  cat("All expected FlexSem output files were found.\n")
  
}

cat("\n")

# --------------------------------------------------------------------------
# Files selected for reading
# --------------------------------------------------------------------------

cat("Files selected for reading:\n\n")

for (name in names(emb_files)) {
  
  cat("---------------------------------------------\n")
  cat(name, "\n")
  cat("---------------------------------------------\n")
  
  for (f in emb_files[[name]]) {
    
    cat("  -", f, "\n")
    
  }
  
  cat("\n")
  
}
# --------------------------------------------------------------------------
# Extraction settings
# --------------------------------------------------------------------------

cat("Extraction element :", elem, "\n\n")

cat("Extraction layers:\n")

for (name in names(layers)) {
  
  cat(
    sprintf("  %-22s : %d\n",
            name,
            layers[[name]])
  )
  
}

cat("\n")
cat("=========================================================\n")
cat("CONFIGURATION SUCCESSFULLY COMPLETED\n")
cat("=========================================================\n")
cat("Ready to run: 02_get_DEB.R\n")
cat("=========================================================\n")