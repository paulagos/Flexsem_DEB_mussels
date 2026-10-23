###############################################################################
# plot_sediment.R
#
# Plot flexsems sediment module
###############################################################################

plot_sediment <- function(deb_data,
                            config){
  
  start_plotting_message("Plotting sediments")
  
  #---------------------------------------------------------------------------
  # Load data
  #---------------------------------------------------------------------------
  
  sediment <- deb_data$sediment
  
  #---------------------------------------------------------------------------
  # Output folder
  #---------------------------------------------------------------------------
    out_folder <- create_output_folder(
    config$output_dir,
    "Sediment"
  )
  
  #---------------------------------------------------------------------------
  # Locations
  #---------------------------------------------------------------------------
  
  locations <- names(sediment)
  
  #---------------------------------------------------------------------------
  # Variables
  #---------------------------------------------------------------------------
  
  variables <- names(sediment[[1]])[-1]
  
  #---------------------------------------------------------------------------
  # Loop over variables
  #---------------------------------------------------------------------------
  
  for(variable in variables){
    
    message("Plotting", variable)
    
    info <- get_variable_info(variable)
    
    plot_df <- data.frame()
    
    #-----------------------------------------------------------------------
    # Combine all locations
    #-----------------------------------------------------------------------
    
    for(location in locations){
      
      df <- sediment[[location]]
      
      if (!(variable %in% names(df))) {
        message(
          "      Skipping ",
          location,
          " (variable not available)"
        )
        next
      }
      
      temp <- data.frame(
        
        datetime = df$datetime,
        
        value = df[[variable]],
        
        Location = location
        
      )
      
      plot_df <- rbind(plot_df, temp)
      
    }
    
    #-----------------------------------------------------------------------
    # Plot
    #-----------------------------------------------------------------------
    
    p <- ggplot(
      plot_df,
      aes(
        x = datetime,
        y = value,
        colour = Location
      )
    ) +
      
      geom_line(linewidth = 0.8) +
      
      labs(
        title = info$title,
        x = "",
        y = info$ylab
      ) +
      
      theme_DEB()
    
    #-----------------------------------------------------------------------
    # Save
    #-----------------------------------------------------------------------
    
    save_DEB_plot(
      plot = p,
      filename = paste0(info$file, ".png"),
      folder = out_folder
    )
    
  }
  
  finish_plotting_message()
  
}