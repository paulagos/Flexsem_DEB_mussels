###############################################################################
# 03_plot_DEB_outputsV2.R
#
# Purpose:
# Master plotting script for DEB farm model outputs.
#
# Author: Paulo Lagos
###############################################################################

###############################################################################
# SECTION 1 - Load packages
###############################################################################

library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

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
# Themes
#------------------------------------------------------------------------------
theme_files <- list.files(
  "R_scripts/functions/themes",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(theme_files, source))

#------------------------------------------------------------------------------
# Plot modules
#------------------------------------------------------------------------------
plot_files <- list.files(
  "R_scripts/functions/plots",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(plot_files, source))

###############################################################################
# SECTION 3 - Load configuration
###############################################################################

source("R_scripts/01_get_config_DEBV2.R")

###############################################################################
# OUTPUT FOLDER
###############################################################################

plot_run <- paste0(
  "Figures_",
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
)

output_dir <- file.path(
  master_folder,
  "Figures",
  plot_run
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

config <- list(
  
  master_folder = master_folder,
  
  output_dir = output_dir,
  
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
# SECTION 5 - Generate plots
###############################################################################

message("----------------------------------------")
message("Generating DEB figures")
message("----------------------------------------")

plot_environment(
  deb_data = deb_data,
  config   = config
)

plot_deb_fluxes(
  deb_data = deb_data,
  config   = config
)

plot_deb_mussels(
  deb_data = deb_data,
  config   = config
)

plot_state_variables(
  deb_data = deb_data,
  config   = config
)

plot_mussels_farm(
  deb_data = deb_data,
  config   = config
)

plot_sediment(
  deb_data = deb_data,
  config   = config
)

###############################################################################
# SECTION 6 - Finish
###############################################################################

message("----------------------------------------")
message("Finished generating figures.")
message("----------------------------------------")