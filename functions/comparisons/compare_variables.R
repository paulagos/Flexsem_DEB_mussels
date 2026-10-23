###############################################################################
# compare_variables.R

# Compare variables within one biological group for one location.
###############################################################################

compare_variables <- function(deb_data,
                              location,
                              group){
  
  #---------------------------------------------------------------------------
  # Summary table
  #---------------------------------------------------------------------------
  
  summary_df <- summary_location(
    deb_data = deb_data,
    location = location
  )
  
  #---------------------------------------------------------------------------
  # Keep requested group
  #---------------------------------------------------------------------------
  
  summary_df <- summary_df[
    summary_df$Group == group,
  ]
  
  #---------------------------------------------------------------------------
  # Sort alphabetically
  #---------------------------------------------------------------------------
  
  summary_df <- summary_df[
    order(summary_df$Variable),
  ]
  
  return(summary_df)
  
}