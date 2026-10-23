###############################################################################
# plot_environment.R
#
# Purpose:
# Plot environmental variables.
#
# Spatial dimensions are detected automatically and preserved dimensions
# are represented as separate plotting series.
#
# Author: Paulo Lagos
###############################################################################

plot_environment <- function(deb_data,
                             config) {
  
  start_plotting_message("Plotting environment")
  
  #---------------------------------------------------------------------------
  # Load data
  #---------------------------------------------------------------------------
  
  environment <- deb_data$environment
  
  #---------------------------------------------------------------------------
  # Output folder
  #---------------------------------------------------------------------------
  
  out_folder <- create_output_folder(
    config$output_dir,
    "Environment"
  )
  
  #---------------------------------------------------------------------------
  # Detect dimensions and variables
  #---------------------------------------------------------------------------
  
  structure_info <- get_data_dimensions(
    environment[[1]]
  )
  
  dimensions <- structure_info$dimensions
  
  variables <- structure_info$variables
  
  message(
    "   Preserved dimensions: ",
    if (length(dimensions) == 0) {
      "none"
    } else {
      paste(
        dimensions,
        collapse = ", "
      )
    }
  )
  
  #---------------------------------------------------------------------------
  # Loop over variables
  #---------------------------------------------------------------------------
  
  for (variable in variables) {
    
    message(
      "   Plotting ",
      variable
    )
    
    info <- get_variable_info(
      variable
    )
    
    #-----------------------------------------------------------------------
    # Prepare multidimensional plotting data
    #-----------------------------------------------------------------------
    
    plot_info <- prepare_plot_data(
      data = environment,
      variable = variable
    )
    
    plot_df <- plot_info$data
    
    #-----------------------------------------------------------------------
    # Figure
    #-----------------------------------------------------------------------
    
    p <- ggplot(
      plot_df,
      aes(
        x = datetime,
        y = value,
        colour = Series,
        group = Series
      )
    ) +
      
      geom_line(
        linewidth = 0.8
      ) +
      
      labs(
        title = info$title,
        x = "",
        y = info$ylab,
        colour = plot_info$legend_title
      ) +
      
      theme_DEB()
    
    #-----------------------------------------------------------------------
    # Save
    #-----------------------------------------------------------------------
    
    save_DEB_plot(
      plot = p,
      filename = paste0(
        info$file,
        ".png"
      ),
      folder = out_folder
    )
    
  }
  
  finish_plotting_message()
  
}