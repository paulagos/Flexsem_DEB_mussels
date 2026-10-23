###############################################################################
# PLOT SF VARIABLE
###############################################################################

plot_sf_variable <- function(plot_data,
                             spatial,
                             title = NULL,
                             palette,
                             limits = NULL,
                             legend_title = NULL){
  
  p <- ggplot2::ggplot() +
    
    ggplot2::geom_sf(
      data = plot_data,
      ggplot2::aes(fill = value),
      colour = NA
    ) +
    
    ggplot2::scale_fill_gradientn(
      colours = palette,
      limits = limits,
      name = legend_title
    ) +
    
    ggplot2::geom_sf(
      data = spatial$coastline,
      fill = "lightgrey",
      colour = "black"
    ) +
    
    ggplot2::coord_sf(
      xlim = spatial$bbox[c("xmin","xmax")],
      ylim = spatial$bbox[c("ymin","ymax")]
    ) +
    
    theme_DEB() +
    
    ggplot2::labs(
      title = title
    )
  
  return(p)
  
}