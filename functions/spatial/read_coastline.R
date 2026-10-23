###############################################################################
# READ COASTLINE
###############################################################################

read_coastline <- function(config){
  
  if(is.null(config$coastline_file)){
    return(NULL)
  }
  
  coast <- sf::st_read(
    config$coastline_file,
    quiet = TRUE
  )
  
  return(coast)
  
}