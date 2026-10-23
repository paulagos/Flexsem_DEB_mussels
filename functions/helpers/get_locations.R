###############################################################################
# Returns available model locations
###############################################################################

get_locations <- function(deb_data){
  
  # Ignore metadata
  outputs <- deb_data[
    names(deb_data) != "metadata"
  ]
  
  # Return locations from the first DEB output
  names(outputs[[1]])
  
}