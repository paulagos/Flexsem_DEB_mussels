###############################################################################
# Create output folder
###############################################################################
create_output_folder <- function(base_folder,
                                 subfolder){
  
  folder <- file.path(base_folder,
                      subfolder)
  
  if (!dir.exists(folder))
    dir.create(folder,
               recursive = TRUE)
  
  return(folder)
  
}