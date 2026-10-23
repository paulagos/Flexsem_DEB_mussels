###############################################################################
# FLEXSEM DEB TOOLBOX
# 01A_get_analysis_scope.R
#
# PURPOSE
# -------
# Builds the spatial and vertical analysis scope used to analyse FlexSem DEB
# outputs.
#
# User-facing analysis settings are defined in:
#
#   run_DEB_toolbox.R
#
# The selected ANALYSIS_MODE is translated here into the standard
# analysis_scope object used by the toolbox.
#
# Available modes:
#
#   • whole_farm
#   • by_depth
#   • by_mask
#   • by_mask_depth
#   • custom
#
# Author : Paulo F. Lagos
# Date   : 07 July 2026
###############################################################################

cat("\n")
cat("-----------------------------------------\n")
cat("Analysis scope\n")
cat("-----------------------------------------\n")


###############################################################################
# CHECK ANALYSIS MODE
###############################################################################

if (!exists("ANALYSIS_MODE")) {
  
  stop(
    paste(
      "ANALYSIS_MODE has not been defined.",
      "Set ANALYSIS_MODE in run_DEB_toolbox.R."
    )
  )
  
}


valid_modes <- c(
  "whole_farm",
  "by_depth",
  "by_mask",
  "by_mask_depth",
  "custom"
)


if (!(ANALYSIS_MODE %in% valid_modes)) {
  
  stop(
    paste(
      "Invalid ANALYSIS_MODE:",
      ANALYSIS_MODE,
      "\nValid modes:",
      paste(
        valid_modes,
        collapse = ", "
      )
    )
  )
  
}


###############################################################################
# BUILD ANALYSIS SCOPE
###############################################################################

if (ANALYSIS_MODE == "whole_farm") {
  
  analysis_scope <- list(
    
    masks       = NULL,
    elements    = NULL,
    layers      = NULL,
    depths      = NULL,
    aggregation = "mean",
    preserve    = NULL
    
  )
  
}


if (ANALYSIS_MODE == "by_depth") {
  
  analysis_scope <- list(
    
    masks       = NULL,
    elements    = NULL,
    layers      = NULL,
    depths      = NULL,
    aggregation = "mean",
    preserve    = "Depth"
    
  )
  
}


if (ANALYSIS_MODE == "by_mask") {
  
  analysis_scope <- list(
    
    masks       = NULL,
    elements    = NULL,
    layers      = NULL,
    depths      = NULL,
    aggregation = "mean",
    preserve    = "Mask"
    
  )
  
}


if (ANALYSIS_MODE == "by_mask_depth") {
  
  analysis_scope <- list(
    
    masks       = NULL,
    elements    = NULL,
    layers      = NULL,
    depths      = NULL,
    aggregation = "mean",
    preserve    = c(
      "Mask",
      "Depth"
    )
    
  )
  
}


if (ANALYSIS_MODE == "custom") {
  
  required_settings <- c(
    "CUSTOM_MASKS",
    "CUSTOM_ELEMENTS",
    "CUSTOM_LAYERS",
    "CUSTOM_DEPTHS",
    "CUSTOM_AGGREGATION",
    "CUSTOM_PRESERVE"
  )
  
  
  missing_settings <- required_settings[
    
    !vapply(
      required_settings,
      exists,
      logical(1),
      inherits = TRUE
    )
    
  ]
  
  
  if (length(missing_settings) > 0) {
    
    stop(
      paste(
        "Missing custom analysis settings:",
        paste(
          missing_settings,
          collapse = ", "
        )
      )
    )
    
  }
  
  
  analysis_scope <- list(
    
    masks       = CUSTOM_MASKS,
    elements    = CUSTOM_ELEMENTS,
    layers      = CUSTOM_LAYERS,
    depths      = CUSTOM_DEPTHS,
    aggregation = CUSTOM_AGGREGATION,
    preserve    = CUSTOM_PRESERVE
    
  )
  
}


###############################################################################
# VALIDATE AGGREGATION
###############################################################################

valid_aggregations <- c(
  "none",
  "mean",
  "sum",
  "min",
  "max"
)


if (!(analysis_scope$aggregation %in% valid_aggregations)) {
  
  stop(
    paste(
      "Invalid aggregation method:",
      analysis_scope$aggregation,
      "\nValid methods:",
      paste(
        valid_aggregations,
        collapse = ", "
      )
    )
  )
  
}


###############################################################################
# VALIDATE PRESERVED DIMENSIONS
###############################################################################

valid_dimensions <- c(
  "Mask",
  "Element",
  "Layer",
  "Depth"
)


if (!is.null(analysis_scope$preserve)) {
  
  invalid_dimensions <- setdiff(
    analysis_scope$preserve,
    valid_dimensions
  )
  
  
  if (length(invalid_dimensions) > 0) {
    
    stop(
      paste(
        "Invalid preserved dimensions:",
        paste(
          invalid_dimensions,
          collapse = ", "
        )
      )
    )
    
  }
  
}


###############################################################################
# SUMMARY HELPERS
###############################################################################

format_selection <- function(x) {
  
  if (is.null(x)) {
    return("ALL")
  }
  
  paste(
    x,
    collapse = ", "
  )
  
}


format_preserve <- function(x) {
  
  if (is.null(x)) {
    return("NONE")
  }
  
  paste(
    x,
    collapse = ", "
  )
  
}


###############################################################################
# SUMMARY
###############################################################################

cat("Mode        :", ANALYSIS_MODE, "\n")
cat("Masks       :", format_selection(analysis_scope$masks), "\n")
cat("Elements    :", format_selection(analysis_scope$elements), "\n")
cat("Layers      :", format_selection(analysis_scope$layers), "\n")
cat("Depths      :", format_selection(analysis_scope$depths), "\n")
cat("Aggregation :", analysis_scope$aggregation, "\n")
cat("Preserve    :", format_preserve(analysis_scope$preserve), "\n")