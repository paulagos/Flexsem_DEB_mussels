###############################################################################
# READ OVERLAYS
###############################################################################

read_overlays <- function(config){
  
  overlays <- list()
  
  if(is.null(config$overlay_files)){
    return(overlays)
  }
  
  for(name in names(config$overlay_files)){
    
    file <- config$overlay_files[[name]]
    
    if(file.exists(file)){
      overlays[[name]] <- sf::st_read(
        file,
        quiet = TRUE
      )
    }
    
  }
  
  return(overlays)
  
}