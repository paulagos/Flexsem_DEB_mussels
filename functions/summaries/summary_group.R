###############################################################################
# summary_group.R
#
# Returns summary statistics for all variables within a given group.
###############################################################################

summary_group <- function(deb_data,
                          group){
  
  #---------------------------------------------------------------------------
  # Variables
  #---------------------------------------------------------------------------
  
  variables <- get_variables(group = group)
  
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