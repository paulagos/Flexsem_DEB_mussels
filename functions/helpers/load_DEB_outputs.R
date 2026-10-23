###############################################################################
# Load one DEB output
###############################################################################

load_DEB_output <- function(deb_data,
                            output_name){
  
  if(!output_name %in% names(deb_data)){
    
    stop(
      sprintf(
        "Output '%s' not found.",
        output_name
      )
    )
    
  }
  
  return(deb_data[[output_name]])
  
}