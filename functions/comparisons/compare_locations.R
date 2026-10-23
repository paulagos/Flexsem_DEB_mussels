###############################################################################
# compare_locations.R
#
# Compare one variable across all locations.
###############################################################################

compare_locations <- function(deb_data,
                              variable){
  
  ###############################################################################
  # Summary table
  ###############################################################################
  
  summary_df <- summary_variable(
    deb_data = deb_data,
    variable = variable
  )
  
  ###############################################################################
  # Rank locations
  ###############################################################################
  
  summary_df <- summary_df[
    order(summary_df$Final,
          decreasing = TRUE),
  ]
  
  summary_df$Rank <- seq_len(nrow(summary_df))
  
  best <- summary_df$Final[1]
  
  summary_df$Difference_from_best <- summary_df$Final - best
  
  return(summary_df)
}