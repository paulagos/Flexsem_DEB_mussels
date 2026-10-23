###############################################################################
# build_farm_cells.R
#
# Purpose:
# Build the complete 3D farm spatial definition from all farm mask files
# registered in the project configuration.
#
# Each mask is read with read_farm_mask() and combined into one table.
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

build_farm_cells <- function(project,
                             base_folder) {
  
  #---------------------------------------------------------------------------
  # Check project configuration
  #---------------------------------------------------------------------------
  
  if (is.null(project$farm_masks)) {
    
    stop(
      "No farm masks are defined in project$farm_masks."
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Empty list for mask tables
  #---------------------------------------------------------------------------
  
  mask_tables <- list()
  
  #---------------------------------------------------------------------------
  # Read each farm mask
  #---------------------------------------------------------------------------
  
  for (mask_file in project$farm_masks) {
    
    full_path <- file.path(
      base_folder,
      mask_file
    )
    
    message(
      "Reading farm mask: ",
      mask_file
    )
    
    mask_tables[[mask_file]] <- read_farm_mask(
      mask_file = full_path
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Combine masks
  #---------------------------------------------------------------------------
  
  farm_cells <- do.call(
    rbind,
    mask_tables
  )
  
  rownames(farm_cells) <- NULL
  
  #---------------------------------------------------------------------------
  # Check for overlapping active cells
  #---------------------------------------------------------------------------
  
  duplicated_cells <- duplicated(
    farm_cells[c(
      "Element",
      "Layer"
    )]
  )
  
  if (any(duplicated_cells)) {
    
    warning(
      "Overlapping active Element × Layer cells were found between farm masks."
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(farm_cells)
  
}