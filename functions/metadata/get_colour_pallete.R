###############################################################################
# get_colour_palette.R
#
# Returns the standard colour palette used throughout the DEB plotting package.
###############################################################################

get_colour_palette <- function(){
  
  list(
    
    scenarios = c(
      
      Current = "#000000",
      
      SSP126  = "#1b9e77",
      
      SSP245  = "#d95f02",
      
      SSP370  = "#7570b3",
      
      SSP585  = "#e7298a"
      
    ),
    
    farms = c(
      
      AsVig     = "#1f78b4",
      
      Horsens   = "#33a02c",
      
      Venoe     = "#e31a1c",
      
      Limfjord  = "#ff7f00"
      
    ),
    
    observations = "#000000",
    
    model = "#0072B2"
    
  )
  
}