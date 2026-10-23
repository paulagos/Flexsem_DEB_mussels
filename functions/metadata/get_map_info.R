###############################################################################
# GET MAP INFO
###############################################################################

get_map_info <- function(variable){
  
  maps <- list(
    
    CHL = list(
      layers = 5,
      limits = NULL,
      palette = "algae",
      fps = 7
    )
    
  )
  
  if(!variable %in% names(maps)){
    stop(
      paste(
        "No map information has been defined for variable",
        variable
      )
    )
  }
  
  return(maps[[variable]])
  
}