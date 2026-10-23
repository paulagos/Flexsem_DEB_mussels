###############################################################################
# Save figure
###############################################################################
save_DEB_plot <- function(plot,
                          filename,
                          folder,
                          width = 8,
                          height = 5,
                          dpi = 300){
  
  if (!dir.exists(folder))
    dir.create(folder,
               recursive = TRUE)
  
  ggsave(
    filename = file.path(folder, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  
}