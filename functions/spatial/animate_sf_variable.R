###############################################################################
# ANIMATE SF VARIABLE
###############################################################################

animate_sf_variable <- function(plot_data,
                                spatial,
                                info,
                                fps = 7,
                                width = 800,
                                height = 600){
  
  p <- plot_sf_variable(
    plot_data = plot_data,
    spatial = spatial,
    info = info
  ) +
    
    gganimate::labs(
      title = paste0(info$label, " ({frame_time})")
    ) +
    
    gganimate::transition_time(time)
  
  animation <- gganimate::animate(
    p,
    nframes = length(unique(plot_data$time)),
    fps = fps,
    width = width,
    height = height
  )
  
  return(animation)
  
}