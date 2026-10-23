###############################################################################
# PREPARE SPATIAL TIMESERIES
###############################################################################

prepare_spatial_timeseries <- function(fs,
                                       variable,
                                       timesteps,
                                       layers,
                                       polygons,
                                       fun = sum){
  
  # Get variable index
  header <- getHeader(fs)
  
  var_index <- match(variable, header$itemNames)
  
  if(is.na(var_index)){
    stop(paste("Variable", variable, "not found."))
  }
  
  plot_list <- vector("list", length(timesteps))
  
  for(i in seq_along(timesteps)){
    
    timestep <- timesteps[i]
    
    # Read requested layers
    layer_values <- lapply(layers, function(layer){
      
      readLayer(
        fs,
        var_index,
        timestep,
        layer
      )
      
    })
    
    # Combine layers
    values <- do.call(
      cbind,
      layer_values
    )
    
    values <- apply(
      values,
      1,
      fun,
      na.rm = TRUE
    )
    
    # Join with polygons
    plot_day <- polygons
    
    plot_day$value <- values
    plot_day$time  <- timestep
    
    plot_list[[i]] <- plot_day
    
  }
  
  plot_data <- do.call(
    rbind,
    plot_list
  )
  
  return(plot_data)
  
}