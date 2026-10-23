###############################################################################
# compare_groups.R
#
# Compare biological groups for one location.
###############################################################################

compare_groups <- function(deb_data,
                           location){
  
  summary_df <- summary_location(
    deb_data = deb_data,
    location = location
  )
  
  summary_df <- summary_df[
    order(summary_df$Group,
          summary_df$Variable),
  ]
  
  return(summary_df)
  
}