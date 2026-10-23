###############################################################################
# summary_location.R
#
# Returns summary statistics for all variables at one location.
###############################################################################

summary_location <- function(deb_data,
                             location){
  
  #---------------------------------------------------------------------------
  # Types
  #---------------------------------------------------------------------------
  
  types <- get_types()
  
  #---------------------------------------------------------------------------
  # Summary table
  #---------------------------------------------------------------------------
  
  summary_df <- data.frame()
  
  #---------------------------------------------------------------------------
  # Loop over types
  #---------------------------------------------------------------------------
  
  for(type in types){
    
    variables <- get_variables(type = type)
    
    for(variable in variables){
      
      temp <- summary_variable(
        deb_data = deb_data,
        variable = variable
      )
      
      temp <- temp[temp$Location == location, ]
      
      summary_df <- rbind(
        summary_df,
        temp
      )
      
    }
    
  }
  
  return(summary_df)
  
}