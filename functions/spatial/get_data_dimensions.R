###############################################################################
# get_data_dimensions.R
#
# Purpose:
# Identify spatial dimensions and model variables in a multidimensional
# DEB output dataframe.
#
# Recognised spatial dimensions:
#
#   Mask
#   Element
#   Layer
#   Depth
#
# Output:
#
#   dimensions
#   variables
#
# Author: Paulo Lagos
###############################################################################

get_data_dimensions <- function(data) {
  
  #---------------------------------------------------------------------------
  # Known dimension columns
  #---------------------------------------------------------------------------
  
  known_dimensions <- c(
    "Mask",
    "Element",
    "Layer",
    "Depth"
  )
  
  #---------------------------------------------------------------------------
  # Detect dimensions present in data
  #---------------------------------------------------------------------------
  
  dimensions <- intersect(
    known_dimensions,
    names(data)
  )
  
  #---------------------------------------------------------------------------
  # Detect model variables
  #---------------------------------------------------------------------------
  
  variables <- setdiff(
    names(data),
    c(
      "datetime",
      dimensions
    )
  )
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(
    list(
      dimensions = dimensions,
      variables = variables
    )
  )
  
}