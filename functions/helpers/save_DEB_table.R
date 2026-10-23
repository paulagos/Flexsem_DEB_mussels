###############################################################################
# Save DEB table
###############################################################################

save_DEB_table <- function(table,
                           filename,
                           folder){
  
  write.csv(
    table,
    file = file.path(folder, filename),
    row.names = FALSE
  )
  
}