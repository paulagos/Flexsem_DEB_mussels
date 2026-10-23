###############################################################################
# ANIMATE ENVIRONMENT
###############################################################################

animate_environment <- function(DEB_outputs,
                                config){
  
  start_plotting_message("Animating Environment")
  
  # Spatial data
  spatial <- prepare_spatial_data(config)
  
  # Environment variables
  variables <- get_variables(
    deb_data = DEB_outputs$environment
  )
  
  for (variable in variables) {
    
    message("   Animating ", variable)
    
    info <- get_variable_info(variable)
    
    animation <- animate_variable(
      fs        = DEB_outputs$environment,
      spatial   = spatial,
      variable  = variable,
      timesteps = config$timesteps,
    )
    
    save_animation(
      animation = animation,
      filename = file.path(
        config$output_dir,
        "Animations",
        "Environment",
        paste0(info$file, ".gif")
      )
    )
    
  }
  
  finish_plotting_message()
  
}