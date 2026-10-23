###############################################################################
# FLEXSEM DEB TOOLBOX
# run_DEB_toolbox.R
#
# Purpose:
# Master workflow script for the FLEXSEM DEB Toolbox.
#
# This script executes the complete DEB post-processing workflow:
#
#   1. Load project configuration
#   2. Load model-output configuration
#   3. Build the 3D farm spatial definition
#   4. Load the analysis scope
#   5. Read model outputs
#   6. Generate figures
#   7. Generate tables
#   8. Generate animations
#
# Individual scripts remain fully independent and can still be run
# separately during development.
#
# Author: Paulo F. Lagos
# Created: 03 July 2026
# Updated: 07 July 2026
###############################################################################

###############################################################################
# USER OPTIONS
###############################################################################

#------------------------------------------------------------------------------
# Launch mode
#------------------------------------------------------------------------------
#
# TRUE:
#   Open the interactive launcher and use the selected settings.
#
# FALSE:
#   Use the manual settings defined below.
#
#------------------------------------------------------------------------------

RUN_INTERACTIVE <- TRUE

RUN_PROJECT <- TRUE


#------------------------------------------------------------------------------
# Manual settings
#
# Used only when:
#
#   RUN_INTERACTIVE <- FALSE
#
#------------------------------------------------------------------------------

RUN_READ        <- TRUE
RUN_PLOTS       <- FALSE
RUN_TABLES      <- FALSE
RUN_ANIMATIONS  <- FALSE

ANALYSIS_MODE <- "by_mask_depth"

CUSTOM_MASKS       <- NULL
CUSTOM_ELEMENTS    <- NULL
CUSTOM_LAYERS      <- NULL
CUSTOM_DEPTHS      <- NULL

CUSTOM_AGGREGATION <- "mean"

CUSTOM_PRESERVE    <- NULL


#------------------------------------------------------------------------------
# Interactive launcher
#------------------------------------------------------------------------------

if (RUN_INTERACTIVE) {
  
  source(
    "R_scripts/functions/interface/run_deb_toolbox_interactive.R"
  )
  
  toolbox_settings <- run_deb_toolbox_interactive()
  
  
  #---------------------------------------------------------------------------
  # Stop cleanly if the launcher was cancelled
  #---------------------------------------------------------------------------
  
  if (is.null(toolbox_settings)) {
    
    cat("\n")
    cat("Toolbox launch cancelled.\n")
    cat("\n")
    
    return(
      invisible(NULL)
    )
    
  }
  
  
  #---------------------------------------------------------------------------
  # Apply interactive settings
  #---------------------------------------------------------------------------
  
  RUN_READ <-
    toolbox_settings$RUN_READ
  
  RUN_PLOTS <-
    toolbox_settings$RUN_PLOTS
  
  RUN_TABLES <-
    toolbox_settings$RUN_TABLES
  
  RUN_ANIMATIONS <-
    toolbox_settings$RUN_ANIMATIONS
  
  ANALYSIS_MODE <-
    toolbox_settings$ANALYSIS_MODE
  
  CUSTOM_MASKS <-
    toolbox_settings$CUSTOM_MASKS
  
  CUSTOM_ELEMENTS <-
    toolbox_settings$CUSTOM_ELEMENTS
  
  CUSTOM_LAYERS <-
    toolbox_settings$CUSTOM_LAYERS
  
  CUSTOM_DEPTHS <-
    toolbox_settings$CUSTOM_DEPTHS
  
  CUSTOM_AGGREGATION <-
    toolbox_settings$CUSTOM_AGGREGATION
  
  CUSTOM_PRESERVE <-
    toolbox_settings$CUSTOM_PRESERVE
  
  SELECTED_SIMULATIONS <-
    toolbox_settings$SELECTED_SIMULATIONS
  
  if (!is.null(toolbox_settings$FARM_CELLS)) {
    
    farm_cells <- toolbox_settings$FARM_CELLS
    
  }
  
  cat("\n")
  cat("=============================================================\n")
  cat("SELECTED SIMULATIONS\n")
  cat("=============================================================\n")
  
  print(SELECTED_SIMULATIONS)
  
  cat("=============================================================\n\n")
  
}

###############################################################################
# TOOLBOX ENGINE
###############################################################################

run_deb_toolbox_engine <- function() {

###############################################################################
# START
###############################################################################

start_time <- Sys.time()

cat("\n")
cat("=============================================================\n")
cat("                 FLEXSEM DEB TOOLBOX\n")
cat("=============================================================\n\n")

###############################################################################
# STEP 1 - PROJECT CONFIGURATION
###############################################################################

if (!exists("project", inherits = TRUE)) {
  
  cat("[1/8] Loading project configuration...\n")
  
  source("R_scripts/00_get_project.R")
  
  cat("      ✓ Done\n\n")
  
} else {
  
  cat("[1/8] Project configuration already loaded\n\n")
  
}

###############################################################################
# STEP 2 - MODEL OUTPUT CONFIGURATION
###############################################################################

if (!exists("emb_files", inherits = TRUE)) {
  
  cat("[2/8] Loading model-output configuration...\n")
  
  source("R_scripts/01_get_config_DEBV2.R")
  
  source("R_scripts/01B_get_simulation_catalog.R")
  
  cat("      ✓ Done\n\n")
  
} else {
  
  cat("[2/8] Model-output configuration already loaded\n\n")
  
  source("R_scripts/01B_get_simulation_catalog.R")
  
}

###############################################################################
# STEP 3 - BUILD 3D FARM DEFINITION
###############################################################################

cat("[3/8] Building 3D farm definition...\n")

if (
  
  !RUN_INTERACTIVE ||
  !exists("farm_cells") ||
  is.null(farm_cells)
  
  ) {
  
  #---------------------------------------------------------------------------
  # Source spatial helper functions
  #---------------------------------------------------------------------------
  
  spatial_files <- list.files(
    "R_scripts/functions/spatial",
    pattern = "\\.R$",
    full.names = TRUE
  )
  
  invisible(
    lapply(
      spatial_files,
      source
    )
  )
  
  #---------------------------------------------------------------------------
  # Farm mask base folder
  #---------------------------------------------------------------------------
  
  farm_base_folder <- dirname(
    master_folder
  )
  
  #---------------------------------------------------------------------------
  # Build active farm cells
  #---------------------------------------------------------------------------
  
  farm_cells <- build_farm_cells(
    project = project,
    base_folder = farm_base_folder
  )
  
  cat("\n========== FARM_CELLS ==========\n")
  
  print(class(farm_cells))
  
  print(names(farm_cells))
  
  print(dim(farm_cells))
  
  print(head(farm_cells))
  
  cat("================================\n\n")
  
} else {
  
  cat("      Using farm definition loaded by Custom analysis\n")
  
}

###############################################################################
# STEP 4 - ANALYSIS SCOPE
###############################################################################

cat("[4/8] Loading analysis scope...\n")

source("R_scripts/01A_get_analysis_scope.R")

cat("      ✓ Done\n\n")

###############################################################################
# FILTER SELECTED SIMULATIONS
###############################################################################

if (RUN_INTERACTIVE) {
  
  cat("[Filtering selected simulations...]\n")
  
  #---------------------------------------------------------------------------
  # Keep only selected simulations in the catalog
  #---------------------------------------------------------------------------
  
  selected_rows <-
    
    simulation_catalog$SimulationID %in%
    
    SELECTED_SIMULATIONS
  
  simulation_catalog <-
    
    simulation_catalog[selected_rows, ]
  
  #---------------------------------------------------------------------------
  # Filter all emb file lists using the same indices
  #---------------------------------------------------------------------------
  
  emb_files <-
    
    lapply(
      
      emb_files,
      
      function(x) {
        
        x[selected_rows]
        
      }
      
    )
  
  cat(
    sprintf(
      "Selected simulations : %d\n",
      sum(selected_rows)
    )
  )
  
  cat(
    paste(
      simulation_catalog$Location,
      collapse = ", "
    ),
    "\n\n"
  )
  
}

###############################################################################
# STEP 5 - READ MODEL OUTPUTS
###############################################################################

if (RUN_READ) {
  
  cat("[5/8] Reading FlexSem outputs...\n")
  
  cat("Engine sees farm_cells: ",
      exists("farm_cells"),
      "\n")
  
  cat("Engine environment: ",
      environmentName(environment()),
      "\n")
  
  source("R_scripts/02_get_DEBV2.R",
         local = environment()
         )
  
  cat("      ✓ Done\n\n")
  
}

###############################################################################
# STEP 6 - GENERATE FIGURES
###############################################################################

if (RUN_PLOTS) {
  
  cat("[6/8] Generating figures...\n")
  
  source("R_scripts/03_plot_DEB_outputsV2.R")
  
  cat("      ✓ Done\n\n")
  
}

###############################################################################
# STEP 7 - GENERATE TABLES
###############################################################################

if (RUN_TABLES) {
  
  cat("[7/8] Generating tables...\n")
  
  source("R_scripts/04_create_tables.R")
  
  cat("      ✓ Done\n\n")
  
}

###############################################################################
# STEP 8 - GENERATE ANIMATIONS
###############################################################################

if (RUN_ANIMATIONS) {
  
  cat("[8/8] Generating animations...\n")
  
  source("R_scripts/04_animate_outputs.R")
  
  cat("      ✓ Done\n\n")
  
}

###############################################################################
# FINISH
###############################################################################

elapsed <- difftime(
  Sys.time(),
  start_time,
  units = "mins"
)

cat("=============================================================\n")
cat("               TOOLBOX COMPLETED SUCCESSFULLY\n")
cat("=============================================================\n\n")

cat(
  sprintf(
    "Elapsed time : %.1f minutes\n",
    as.numeric(elapsed)
  )
)

cat("\n")
cat("=============================================================\n")

}

###############################################################################
# LAUNCH TOOLBOX ENGINE
###############################################################################

if (RUN_INTERACTIVE) {
  
  later::later(
    
    function() {
      
      run_deb_toolbox_engine()
      
    },
    
    delay = 0.1
    
  )
  
} else {
  
  run_deb_toolbox_engine()
  
}
