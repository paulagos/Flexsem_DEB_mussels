###############################################################################
# prepare_plot_data.R
#
# Purpose:
# Prepare multidimensional DEB output data for plotting.
#
# The function:
#
#   1. Detects preserved spatial dimensions
#   2. Detects model variables
#   3. Extracts one requested variable
#   4. Combines all model locations
#   5. Creates a unique plotting series for each combination of
#      Location × preserved dimensions
#
# Output:
#
#   data
#   dimensions
#   variables
#   legend_title
#
# Author: Paulo Lagos
###############################################################################

prepare_plot_data <- function(data,
                              variable) {
  
  #---------------------------------------------------------------------------
  # Detect dimensions and variables
  #---------------------------------------------------------------------------
  
  structure_info <- get_data_dimensions(
    data[[1]]
  )
  
  dimensions <- structure_info$dimensions
  
  variables <- structure_info$variables
  
  #---------------------------------------------------------------------------
  # Check requested variable
  #---------------------------------------------------------------------------
  
  if (!(variable %in% variables)) {
    
    stop(
      paste(
        "Variable",
        variable,
        "is not available in this output."
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Locations
  #---------------------------------------------------------------------------
  
  locations <- names(data)
  
  #---------------------------------------------------------------------------
  # Combined plotting dataframe
  #---------------------------------------------------------------------------
  
  plot_df <- data.frame()
  
  #---------------------------------------------------------------------------
  # Combine locations
  #---------------------------------------------------------------------------
  
  for (location in locations) {
    
    df <- data[[location]]
    
    if (!(variable %in% names(df))) {
      
      message(
        "      Skipping ",
        location,
        " (variable not available)"
      )
      
      next
      
    }
    
    #-------------------------------------------------------------------------
    # Detect dimensions available at this location
    #-------------------------------------------------------------------------
    
    location_dimensions <- intersect(
      dimensions,
      names(df)
    )
    
    #-------------------------------------------------------------------------
    # Keep required columns
    #-------------------------------------------------------------------------
    
    keep_columns <- c(
      "datetime",
      location_dimensions,
      variable
    )
    
    temp <- df[
      ,
      keep_columns,
      drop = FALSE
    ]
    
    #-------------------------------------------------------------------------
    # Standardise variable name
    #-------------------------------------------------------------------------
    
    names(temp)[
      names(temp) == variable
    ] <- "value"
    
    #-------------------------------------------------------------------------
    # Add location
    #-------------------------------------------------------------------------
    
    temp$Location <- location
    
    #-------------------------------------------------------------------------
    # Combine
    #-------------------------------------------------------------------------
    
    plot_df <- rbind(
      plot_df,
      temp
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Check data were found
  #---------------------------------------------------------------------------
  
  if (nrow(plot_df) == 0) {
    
    stop(
      paste(
        "No plotting data found for variable:",
        variable
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Create plotting series
  #---------------------------------------------------------------------------
  
  if (length(dimensions) == 0) {
    
    plot_df$Series <- plot_df$Location
    
    legend_title <- "Location"
    
  } else {
    
    series_columns <- c(
      "Location",
      dimensions
    )
    
    plot_df$Series <- apply(
      
      plot_df[
        ,
        series_columns,
        drop = FALSE
      ],
      
      1,
      
      function(x) {
        
        paste(
          x,
          collapse = " | "
        )
        
      }
      
    )
    
    legend_title <- paste(
      series_columns,
      collapse = " × "
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(
    list(
      data = plot_df,
      dimensions = dimensions,
      variables = variables,
      legend_title = legend_title
    )
  )
  
}