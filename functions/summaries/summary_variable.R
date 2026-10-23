###############################################################################
# summary_variable.R
#
# Returns summary statistics for one DEB output variable.
###############################################################################

summary_variable <- function(deb_data,
                             variable){
  
  #---------------------------------------------------------------------------
  # Variable metadata
  #---------------------------------------------------------------------------
  
  info <- get_variable_info(variable)
  
  #---------------------------------------------------------------------------
  # Select data source
  #---------------------------------------------------------------------------
  
  data <- deb_data[[info$type]]
  
  #---------------------------------------------------------------------------
  # Locations
  #---------------------------------------------------------------------------
  
  locations <- names(data)
  
  #---------------------------------------------------------------------------
  # Summary table
  #---------------------------------------------------------------------------
  
  summary_df <- data.frame()
  
  #---------------------------------------------------------------------------
  # Loop over locations
  #---------------------------------------------------------------------------
  
  for(location in locations){
    
    df <- data[[location]]
    
    if(!(variable %in% names(df))){
      
      next
      
    }
    
    values <- df[[variable]]
    
    temp <- data.frame(
      
      Variable = variable,
      
      Group = info$group,
      
      Unit = info$unit,
      
      Location = location,
      
      Mean = mean(values, na.rm = TRUE),
      
      Min = min(values, na.rm = TRUE),
      
      Max = max(values, na.rm = TRUE),
      
      Final = tail(values, 1)
      
    )
    
    summary_df <- rbind(summary_df, temp)
    
  }
  
  return(summary_df)
  
}