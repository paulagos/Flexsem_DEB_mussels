###############################################################################
# ANIMATE VARIABLE
###############################################################################

animate_variable <- function(fs,
                             spatial,
                             variable,
                             timesteps,
                             width = 800,
                             height = 600){
  
  # Variable metadata
  info <- get_variable_info(variable)
  map  <- get_map_info(variable)
  
  # Prepare spatial data
  plot_data <- prepare_spatial_timeseries(
    fs        = fs,
    variable  = variable,
    timesteps = timesteps,
    layers    = map$layers,
    polygons  = spatial$polygons
  )
  
  # Create animation
  animation <- animate_sf_variable(
    plot_data = plot_data,
    spatial   = spatial,
    info      = info,
    fps       = map$fps,
    width     = width,
    height    = height
  )
  
  return(animation)
  
}