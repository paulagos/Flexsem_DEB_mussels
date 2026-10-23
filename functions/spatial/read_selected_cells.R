###############################################################################
# read_selected_cells.R
#
# Purpose:
# Read one FlexSem variable for a selected set of active farm cells.
#
# Each selected farm cell is defined by:
#
#   Element × Layer × Depth × Mask
#
# The function preserves the spatial dimensions and returns one long-format
# dataframe containing:
#
#   datetime
#   value
#   Mask
#   Element
#   Layer
#   Depth
#
# No spatial aggregation is performed here.
#
# Author: Paulo Lagos
###############################################################################

read_selected_cells <- function(file,
                                variable,
                                selected_cells) {
  
  #---------------------------------------------------------------------------
  # Check file
  #---------------------------------------------------------------------------
  
  if (!file.exists(file)) {
    
    stop(
      paste(
        "FlexSem file not found:",
        file
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Check selected cells
  #---------------------------------------------------------------------------
  
  required_columns <- c(
    "Mask",
    "Element",
    "Layer",
    "Depth"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(selected_cells)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      paste(
        "selected_cells is missing required columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
    
  }
  
  if (nrow(selected_cells) == 0) {
    
    stop(
      "selected_cells contains no active farm cells."
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Open FlexSem file
  #---------------------------------------------------------------------------
  
  fs <- openFSFile(file)
  
  header <- getHeader(fs)
  
  #---------------------------------------------------------------------------
  # Find variable
  #---------------------------------------------------------------------------
  
  item_index <- match(
    variable,
    header$itemNames
  )
  
  if (is.na(item_index)) {
    
    stop(
      paste(
        "Variable not found:",
        variable
      )
    )
    
  }
  
  # FlexSem uses zero-based item numbering
  item_index <- item_index - 1
  
  #---------------------------------------------------------------------------
  # Empty output
  #---------------------------------------------------------------------------
  
  output <- data.frame()
  
  #---------------------------------------------------------------------------
  # Read selected cells
  #---------------------------------------------------------------------------
  
  for (i in seq_len(nrow(selected_cells))) {
    
    cell <- selected_cells[i, ]
    
    ts <- fsReadTimeseries(
      
      fs,
      
      item_index,
      
      cell$Element,
      
      cell$Layer
      
    )
    
    temp <- data.frame(
      
      datetime = ts[, 1],
      
      value = ts[, 2],
      
      Mask = cell$Mask,
      
      Element = cell$Element,
      
      Layer = cell$Layer,
      
      Depth = cell$Depth,
      
      stringsAsFactors = FALSE
      
    )
    
    output <- rbind(
      output,
      temp
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Sort output
  #---------------------------------------------------------------------------
  
  output <- output[
    order(
      output$datetime,
      output$Mask,
      output$Element,
      output$Layer
    ),
  ]
  
  rownames(output) <- NULL
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(output)
  
}