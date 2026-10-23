###############################################################################
# get_groups.R
#
# Returns all variable groups available in the metadata.
###############################################################################

get_groups <- function(){
  
  metadata <- get_variable_info()
  
  groups <- sapply(
    metadata,
    function(x) x$group
  )
  
  groups <- unique(groups)
  
  groups <- sort(groups)
  
  return(groups)
  
}