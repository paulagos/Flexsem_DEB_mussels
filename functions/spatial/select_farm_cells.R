###############################################################################
# select_farm_cells.R
#
# Purpose:
# Select a subset of active farm cells from the farm spatial definition.
#
# Farm cells can be filtered by:
#
#   • Mask
#   • Element
#   • Layer
#   • Depth
#
# Multiple values can be supplied to any filter.
#
# If a filter is NULL, all available values are retained.
#
# Examples:
#
#   One cell:
#     elements = 212
#     layers   = 5
#
#   One element across all active depths:
#     elements = 212
#
#   One mask:
#     masks = "farmmask2"
#
#   One depth across the whole farm:
#     depths = 0.5
#
#   Whole farm:
#     all filters = NULL
#
# Author: Paulo Lagos
###############################################################################

select_farm_cells <- function(farm_cells,
                              masks = NULL,
                              elements = NULL,
                              layers = NULL,
                              depths = NULL) {
  
  #---------------------------------------------------------------------------
  # Check input
  #---------------------------------------------------------------------------
  
  required_columns <- c(
    "Mask",
    "Element",
    "Layer",
    "Depth",
    "MaskValue"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(farm_cells)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      paste(
        "farm_cells is missing required columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Start with complete farm
  #---------------------------------------------------------------------------
  
  selected_cells <- farm_cells
  
  #---------------------------------------------------------------------------
  # Filter by mask
  #---------------------------------------------------------------------------
  
  if (!is.null(masks)) {
    
    selected_cells <- selected_cells[
      selected_cells$Mask %in% masks,
    ]
    
  }
  
  #---------------------------------------------------------------------------
  # Filter by element
  #---------------------------------------------------------------------------
  
  if (!is.null(elements)) {
    
    selected_cells <- selected_cells[
      selected_cells$Element %in% elements,
    ]
    
  }
  
  #---------------------------------------------------------------------------
  # Filter by layer
  #---------------------------------------------------------------------------
  
  if (!is.null(layers)) {
    
    selected_cells <- selected_cells[
      selected_cells$Layer %in% layers,
    ]
    
  }
  
  #---------------------------------------------------------------------------
  # Filter by depth
  #---------------------------------------------------------------------------
  
  if (!is.null(depths)) {
    
    selected_cells <- selected_cells[
      selected_cells$Depth %in% depths,
    ]
    
  }
  
  #---------------------------------------------------------------------------
  # Check result
  #---------------------------------------------------------------------------
  
  if (nrow(selected_cells) == 0) {
    
    warning(
      "No active farm cells matched the requested selection."
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Sort result
  #---------------------------------------------------------------------------
  
  selected_cells <- selected_cells[
    order(
      selected_cells$Mask,
      selected_cells$Element,
      selected_cells$Layer
    ),
  ]
  
  rownames(selected_cells) <- NULL
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(selected_cells)
  
}