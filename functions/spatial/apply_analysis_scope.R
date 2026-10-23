###############################################################################
# apply_analysis_scope.R
#
# Purpose:
# Apply a user-defined analysis scope to a FlexSem variable.
#
# The analysis scope controls:
#
#   1. Which active farm cells are selected
#   2. Which variable is read
#   3. How the selected data are aggregated
#   4. Which spatial dimensions are preserved
#
# Required analysis_scope fields:
#
#   masks
#   elements
#   layers
#   depths
#   aggregation
#   preserve
#
# Example:
#
#   analysis_scope <- list(
#     masks       = "farmmask2",
#     elements    = NULL,
#     layers      = NULL,
#     depths      = NULL,
#     aggregation = "mean",
#     preserve    = "Depth"
#   )
#
# Author: Paulo Lagos
###############################################################################

apply_analysis_scope <- function(file,
                                 variable,
                                 farm_cells,
                                 analysis_scope) {
  
  #---------------------------------------------------------------------------
  # Check analysis scope
  #---------------------------------------------------------------------------
  
  required_fields <- c(
    "masks",
    "elements",
    "layers",
    "depths",
    "aggregation",
    "preserve"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(analysis_scope)
  )
  
  if (length(missing_fields) > 0) {
    
    stop(
      paste(
        "analysis_scope is missing required fields:",
        paste(missing_fields, collapse = ", ")
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Select active farm cells
  #---------------------------------------------------------------------------
  
  selected_cells <- select_farm_cells(
    
    farm_cells = farm_cells,
    
    masks = analysis_scope$masks,
    
    elements = analysis_scope$elements,
    
    layers = analysis_scope$layers,
    
    depths = analysis_scope$depths
    
  )
  
  #---------------------------------------------------------------------------
  # Read selected cells
  #---------------------------------------------------------------------------
  
  selected_data <- read_selected_cells(
    
    file = file,
    
    variable = variable,
    
    selected_cells = selected_cells
    
  )
  
  #---------------------------------------------------------------------------
  # Aggregate selected data
  #---------------------------------------------------------------------------
  
  output <- aggregate_selected_cells(
    
    data = selected_data,
    
    method = analysis_scope$aggregation,
    
    group_by = analysis_scope$preserve
    
  )
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(output)
  
}