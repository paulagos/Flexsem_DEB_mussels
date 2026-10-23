###############################################################################
# summary_type.R
#
# Returns summary statistics for all variables of a given output type.
###############################################################################

summary_type <- function(deb_data,
                         type){
  
  #---------------------------------------------------------------------------
  # Variables
  #---------------------------------------------------------------------------
  
  variables <- get_variables(type = type)
  
  #---------------------------------------------------------------------------
  # Summary table
  #---------------------------------------------------------------------------
  
  summary_df <- data.frame()
  
  #---------------------------------------------------------------------------
  # Loop over variables
  #---------------------------------------------------------------------------
  
  for(variable in variables){
    
    temp <- summary_variable(
      deb_data = deb_data,
      variable = variable
    )
    
    summary_df <- rbind(
      summary_df,
      temp
    )
    
  }
  
  return(summary_df)
  
}