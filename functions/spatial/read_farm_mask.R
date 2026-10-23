###############################################################################
# read_farm_mask.R
#
# Purpose:
# Read a FlexSem farm mask and return a table containing all active
# element × layer cells occupied by mussels.
#
# The farm mask is treated as a three-dimensional spatial definition:
#
#   element × layer × depth
#
# Only cells with a mask value greater than zero are returned.
#
# Output columns:
#
#   Mask
#   Element
#   Layer
#   Depth
#   MaskValue
#
# Author: Paulo Lagos
###############################################################################

read_farm_mask <- function(mask_file,
                           mask_name = NULL) {
  
  #---------------------------------------------------------------------------
  # Check file
  #---------------------------------------------------------------------------
  
  if (!file.exists(mask_file)) {
    
    stop(
      paste(
        "Farm mask file not found:",
        mask_file
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Open FlexSem file
  #---------------------------------------------------------------------------
  
  fs <- openFSFile(mask_file)
  
  header <- getHeader(fs)
  
  #---------------------------------------------------------------------------
  # Basic validation
  #---------------------------------------------------------------------------
  
  if (length(header$itemNames) != 1) {
    
    stop(
      paste(
        "Expected one variable in farm mask but found:",
        length(header$itemNames)
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Mask name
  #---------------------------------------------------------------------------
  
  if (is.null(mask_name)) {
    
    mask_name <- header$itemNames[1]
    
  }
  
  #---------------------------------------------------------------------------
  # Empty output table
  #---------------------------------------------------------------------------
  
  active_cells <- data.frame(
    
    Mask = character(),
    
    Element = integer(),
    
    Layer = integer(),
    
    Depth = numeric(),
    
    MaskValue = numeric(),
    
    stringsAsFactors = FALSE
    
  )
  
  #---------------------------------------------------------------------------
  # Read every element × layer combination
  #---------------------------------------------------------------------------
  
  for (element in 0:(header$noElem - 1)) {
    
    for (layer in 0:(header$noLayer - 1)) {
      
      x <- fsReadTimeseries(
        
        fs,
        
        0,
        
        element,
        
        layer
        
      )
      
      value <- x[1, 2]
      
      #-----------------------------------------------------------------------
      # Keep active farm cells only
      #-----------------------------------------------------------------------
      
      if (!is.na(value) && value > 0) {
        
        active_cells <- rbind(
          
          active_cells,
          
          data.frame(
            
            Mask = mask_name,
            
            Element = element,
            
            Layer = layer,
            
            Depth = header$layerDepth[layer + 1],
            
            MaskValue = value,
            
            stringsAsFactors = FALSE
            
          )
          
        )
        
      }
      
    }
    
  }
  
  #---------------------------------------------------------------------------
  # Sort output
  #---------------------------------------------------------------------------
  
  active_cells <- active_cells[
    
    order(
      active_cells$Element,
      active_cells$Layer
    ),
    
  ]
  
  rownames(active_cells) <- NULL
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(active_cells)
  
}