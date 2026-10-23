###############################################################################
# get_types.R
#
# Returns all variable types available in the metadata.
###############################################################################

get_types <- function(){
  
  metadata <- get_variable_info()
  
  types <- sapply(
    metadata,
    function(x) x$type
  )
  
  types <- unique(types)
  
  types <- sort(types)
  
  return(types)
  
}