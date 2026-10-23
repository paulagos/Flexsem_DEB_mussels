###############################################################################
# Publication theme
###############################################################################

theme_DEB <- function() {
  
  theme_bw(base_size = 12) +
    
    theme(
      
      panel.grid.major = element_line(colour = "grey90"),
      
      panel.grid.minor = element_blank(),
      
      axis.title = element_text(face = "bold"),
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      
      legend.position = "bottom",
      
      legend.title = element_blank()
      
    )
  
}