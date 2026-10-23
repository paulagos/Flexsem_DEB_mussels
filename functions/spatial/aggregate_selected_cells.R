###############################################################################
# aggregate_selected_cells.R
#
# Purpose:
# Aggregate multidimensional FlexSem data after spatial selection.
#
# The input data are expected to contain:
#
#   datetime
#   value
#   Mask
#   Element
#   Layer
#   Depth
#
# The function allows the user to:
#
#   • Preserve all spatial dimensions
#   • Average or sum across selected cells
#   • Preserve selected grouping dimensions such as:
#       - Mask
#       - Element
#       - Layer
#       - Depth
#
# Examples:
#
#   Preserve all cells:
#     method = "none"
#
#   Whole selection mean:
#     method = "mean"
#     group_by = NULL
#
#   Mean by depth:
#     method = "mean"
#     group_by = "Depth"
#
#   Mean by mask and depth:
#     method = "mean"
#     group_by = c("Mask", "Depth")
#
#   Sum by mask:
#     method = "sum"
#     group_by = "Mask"
#
# No assumption is made about which aggregation method is scientifically
# appropriate for a particular variable.
#
# Author: Paulo Lagos
###############################################################################

aggregate_selected_cells <- function(data,
                                     method = "none",
                                     group_by = NULL) {
  
  #---------------------------------------------------------------------------
  # Check input
  #---------------------------------------------------------------------------
  
  required_columns <- c(
    "datetime",
    "value",
    "Mask",
    "Element",
    "Layer",
    "Depth"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      paste(
        "data is missing required columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Check method
  #---------------------------------------------------------------------------
  
  valid_methods <- c(
    "none",
    "mean",
    "sum",
    "min",
    "max"
  )
  
  if (!(method %in% valid_methods)) {
    
    stop(
      paste(
        "Invalid aggregation method:",
        method,
        "\nValid methods:",
        paste(valid_methods, collapse = ", ")
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # No aggregation
  #---------------------------------------------------------------------------
  
  if (method == "none") {
    
    return(data)
    
  }
  
  #---------------------------------------------------------------------------
  # Check grouping variables
  #---------------------------------------------------------------------------
  
  if (!is.null(group_by)) {
    
    invalid_groups <- setdiff(
      group_by,
      c(
        "Mask",
        "Element",
        "Layer",
        "Depth"
      )
    )
    
    if (length(invalid_groups) > 0) {
      
      stop(
        paste(
          "Invalid grouping variables:",
          paste(invalid_groups, collapse = ", ")
        )
      )
      
    }
    
  }
  
  #---------------------------------------------------------------------------
  # Define grouping columns
  #---------------------------------------------------------------------------
  
  grouping_columns <- c(
    "datetime",
    group_by
  )
  
  #---------------------------------------------------------------------------
  # Aggregate
  #---------------------------------------------------------------------------
  
  output <- data |>
    
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(grouping_columns)
      )
    ) |>
    
    dplyr::summarise(
      
      value = switch(
        
        method,
        
        mean = mean(value, na.rm = TRUE),
        
        sum = sum(value, na.rm = TRUE),
        
        min = min(value, na.rm = TRUE),
        
        max = max(value, na.rm = TRUE)
        
      ),
      
      .groups = "drop"
      
    )
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(
    as.data.frame(output)
  )
  
}