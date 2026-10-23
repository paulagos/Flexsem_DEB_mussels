###############################################################################
# FLEXSEM DEB TOOLBOX
# 00_get_project.R
###############################################################################
#
# PURPOSE or wtf I am doing here
# -------
# This script defines the geographic project used by the DEB Toolbox.
# A project represents a modelling domain (e.g. Danish Inland Waters,
# North Sea, Baltic Sea) and contains all static spatial information that
# is common to every model run performed within that domain.
#
# Unlike individual model runs, which describe a single farm simulation,
# the project contains information that does not change between farms,
# including:
#
#   • Computational mesh
#   • Coordinate reference system (CRS)
#   • Coastline shapefiles
#   • Spatial overlays (farms, stations, wind farms, etc.)
#   • Other geographic datasets required for spatial analyses
#
# The project object is used by the spatial engine of the toolbox,
# including map generation, animations and future GIS analyses.
# It is intentionally independent from the DEB configuration and model
# outputs, allowing multiple farm simulations to share the same spatial
# framework.
#
# TOOLBOX WORKFLOW
# ----------------
# 00_get_project.R      -> Define the geographic project
# 01_get_config_DEBV2.R -> Configure model outputs
# 02_get_DEBV2.R        -> Read DEB outputs
# 03_plot_DEB_outputs.R -> Generate figures
# 04_animate_outputs.R  -> Generate animations
#
###############################################################################
#
# Author : Paulo Lagos
# Date   : 29 June 2026
#
###############################################################################

cat("\n")
cat("-----------------------------------------\n")
cat("Project configuration\n")
cat("-----------------------------------------\n")

###############################################################################
# PROJECT
###############################################################################

project <- list(
  
  # Project name
  name = "Danish Inland Waters",
  
  # Coordinate reference system
  crs = 25832,
  
  # Mesh
  mesh_file = "musselFarmV3.mesh",
  
  # Farm masks
  farm_masks = c(
    "farmMask1.emb",
    "farmMask2.emb"
  ),
  
  # Coastline
  coastline_file = NULL,
  
  # Optional overlays
  overlays = NULL
  
)

cat("Project :", project$name, "\n")
cat("Mesh    :", project$mesh_file, "\n")
cat(
  "Farm masks :",
  paste(project$farm_masks, collapse = ", "),
  "\n"
)