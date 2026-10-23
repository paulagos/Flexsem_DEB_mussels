###############################################################################
# get_config_DEB.R
#
# Read the DEB project configuration and locate all FlexSem output files.
#
# Returns:
#   config
###############################################################################

get_config_DEB <- function(){
  
  source("C:/AU/R/emfunc.r")
  
  library(FlexsemTools)
  
  #---------------------------------------------------------------------------
  # USER SETTINGS
  #---------------------------------------------------------------------------
  
  master_folder <- "C:/AU/farmsIDWNS2026/IDW/2019/DEB_outputs"
  
  results_mode <- "latest"
  
  results_folder_name <- "results_june_23"
  
  elem <- 0
  
  layers <- list(
    deb_mussels         = 5,
    deb_fluxes          = 5,
    deb_state_variables = 5,
    environment         = 5,
    mussels_farm        = 5,
    sediment            = 0
  )
  
  output_files <- c(
    "deb_mussels.emb",
    "deb_fluxes.emb",
    "deb_state_variables.emb",
    "environment.emb",
    "mussels_farm.emb",
    "sediment.emb"
  )
  
  ###########################################################################
  ## EVERYTHING FROM YOUR CURRENT SCRIPT GOES HERE
  ###########################################################################
  
  config <- list(
    
    master_folder       = master_folder,
    
    results_mode        = results_mode,
    
    results_folder_name = results_folder_name,
    
    elem                = elem,
    
    layers              = layers,
    
    output_files        = output_files,
    
    emb_files           = emb_files,
    
    loaded_locations    = loaded_locations,
    
    missing_locations   = missing_locations
    
  )
  
  return(config)
  
}