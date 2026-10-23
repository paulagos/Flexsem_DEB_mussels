###############################################################################
# read_aggregated_cells.R
#
# Purpose:
# Read and aggregate one FlexSem variable directly from selected farm cells
# without creating a large intermediate long-format dataframe.
#
# This function is designed for memory-efficient spatial aggregation.
#
# Supported methods:
#
#   mean
#   sum
#   min
#   max
#
# Supported preserved dimensions:
#
#   NULL
#   Mask
#   Element
#   Layer
#   Depth
#   or combinations of these dimensions
#
# Examples:
#
#   Whole selection mean:
#     method = "mean"
#     group_by = NULL
#
#   Mean by depth:
#     method = "mean"
#     group_by = "Depth"
#
#   Mean by mask and depth:
#     method = "mean"
#     group_by = c("Mask", "Depth")
#
# Author: Paulo Lagos
###############################################################################

read_aggregated_cells <- function(file,
                                  variable,
                                  selected_cells,
                                  method = "mean",
                                  group_by = NULL) {
  #---------------------------------------------------------------------------
  # Check file
  #---------------------------------------------------------------------------
  
  if (!file.exists(file)) {
    
    stop(
      paste(
        "FlexSem file not found:",
        file
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Check selected cells
  #---------------------------------------------------------------------------
  
  required_columns <- c(
    "Mask",
    "Element",
    "Layer",
    "Depth"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(selected_cells)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      paste(
        "selected_cells is missing required columns:",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
    
  }
  
  if (nrow(selected_cells) == 0) {
    
    stop(
      "selected_cells contains no active farm cells."
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Check aggregation method
  #---------------------------------------------------------------------------
  
  valid_methods <- c(
    "mean",
    "sum",
    "min",
    "max"
  )
  
  if (!(method %in% valid_methods)) {
    
    stop(
      paste(
        "Invalid aggregation method:",
        method,
        "\nValid methods:",
        paste(
          valid_methods,
          collapse = ", "
        )
      )
    )
    
  }
  
  #---------------------------------------------------------------------------
  # Check preserved dimensions
  #---------------------------------------------------------------------------
  
  valid_dimensions <- c(
    "Mask",
    "Element",
    "Layer",
    "Depth"
  )
  
  if (!is.null(group_by)) {
    
    invalid_groups <- setdiff(
      group_by,
      valid_dimensions
    )
    
    if (length(invalid_groups) > 0) {
      
      stop(
        paste(
          "Invalid grouping variables:",
          paste(
            invalid_groups,
            collapse = ", "
          )
        )
      )
      
    }
    
  }
  
  #---------------------------------------------------------------------------
  # Open FlexSem file
  #---------------------------------------------------------------------------
  
  fs <- openFSFile(file)
  
  header <- getHeader(fs)
  
  #---------------------------------------------------------------------------
  # Find variable
  #---------------------------------------------------------------------------
  
  item_index <- match(
    variable,
    header$itemNames
  )
  
  if (is.na(item_index)) {
    
    stop(
      paste(
        "Variable not found:",
        variable
      )
    )
    
  }
  
  # FlexSem uses zero-based item numbering
  
  item_index <- item_index - 1
  
  #---------------------------------------------------------------------------
  # Build aggregation groups
  #---------------------------------------------------------------------------
  
  if (is.null(group_by)) {
    
    selected_cells$.group_id <- "ALL"
    
  } else {
    
    selected_cells$.group_id <- interaction(
      
      selected_cells[group_by],
      
      drop = TRUE,
      
      lex.order = TRUE
      
    )
    
  }
  
  group_ids <- unique(
    selected_cells$.group_id
  )
  
  #---------------------------------------------------------------------------
  # Output storage
  #---------------------------------------------------------------------------
  
  output_list <- vector(
    "list",
    length(group_ids)
  )
  
  #---------------------------------------------------------------------------
  # Process one aggregation group at a time
  #---------------------------------------------------------------------------
  
  for (g in seq_along(group_ids)) {
    
    group_id <- group_ids[g]
    
    group_cells <- selected_cells[
      selected_cells$.group_id == group_id,
      ,
      drop = FALSE
    ]
    
    # Storage created after reading first cell
    
    datetime <- NULL
    
    accumulator <- NULL
    
    valid_count <- NULL
    
    #-----------------------------------------------------------------------
    # Read cells in current group
    #-----------------------------------------------------------------------
    
    for (i in seq_len(nrow(group_cells))) {
      
      cell <- group_cells[i, ]
      
      ts <- FlexsemTools::fsReadTimeseries(
        fs,
        item_index,
        cell$Element,
        cell$Layer
      )
      
     current_datetime <- ts[, 1]
      
     current_value <- ts[, 2]
     
     #---------------------------------------------------------------------
     # Temporal aggregation for high-frequency state-variable outputs
     #---------------------------------------------------------------------
     
     if (basename(file) == "deb_state_variables.emb") {
       
       current_date <- as.Date(current_datetime)
       
       daily_data <- aggregate(
         current_value,
         by = list(date = current_date),
         FUN = mean,
         na.rm = TRUE
       )
       
       current_datetime <- as.POSIXct(
         daily_data$date,
         tz = "GMT"
       )
       
       current_value <- daily_data$x
     }
     
      
      #---------------------------------------------------------------------
      # Initialize accumulator
      #---------------------------------------------------------------------
      
      if (is.null(accumulator)) {
        
        datetime <- current_datetime
        
        if (method %in% c("mean", "sum")) {
          
          accumulator <- rep(
            0,
            length(current_value)
          )
          
        }
        
        if (method == "min") {
          
          accumulator <- rep(
            Inf,
            length(current_value)
          )
          
        }
        
        if (method == "max") {
          
          accumulator <- rep(
            -Inf,
            length(current_value)
          )
          
        }
        
        valid_count <- rep(
          0L,
          length(current_value)
        )
        
      }
      
      #---------------------------------------------------------------------
      # Check timestep consistency
      #---------------------------------------------------------------------
      
      if (length(current_datetime) != length(datetime)) {
        
        stop(
          paste(
            "Timeseries length mismatch for element",
            cell$Element,
            "layer",
            cell$Layer
          )
        )
        
      }
      
      #---------------------------------------------------------------------
      # Valid values
      #---------------------------------------------------------------------
      
      valid <- !is.na(current_value)
      
      #---------------------------------------------------------------------
      # Update accumulator
      #---------------------------------------------------------------------
      
      if (method %in% c("mean", "sum")) {
        
        accumulator[valid] <-
          accumulator[valid] +
          current_value[valid]
        
      }
      
      if (method == "min") {
        
        accumulator[valid] <- pmin(
          accumulator[valid],
          current_value[valid]
        )
        
      }
      
      if (method == "max") {
        
        accumulator[valid] <- pmax(
          accumulator[valid],
          current_value[valid]
        )
        
      }
      
      valid_count[valid] <-
        valid_count[valid] + 1L
      
    }
    
    #-----------------------------------------------------------------------
    # Finalise aggregation
    #-----------------------------------------------------------------------
    
    if (method == "mean") {
      
      final_value <- accumulator / valid_count
      
    } else {
      
      final_value <- accumulator
      
    }
    
    final_value[
      valid_count == 0
    ] <- NA_real_
    
    #-----------------------------------------------------------------------
    # Build group output
    #-----------------------------------------------------------------------
    
    group_output <- data.frame(
      
      datetime = datetime,
      
      value = final_value,
      
      stringsAsFactors = FALSE
      
    )
    
    #-----------------------------------------------------------------------
    # Restore preserved dimensions
    #-----------------------------------------------------------------------
    
    if (!is.null(group_by)) {
      
      for (dimension in group_by) {
        
        group_output[[dimension]] <-
          group_cells[[dimension]][1]
        
      }
      
    }
    
    output_list[[g]] <- group_output
    
  }
  
  #---------------------------------------------------------------------------
  # Combine groups
  #---------------------------------------------------------------------------
  
  output <- dplyr::bind_rows(
    output_list
  )
  
  #---------------------------------------------------------------------------
  # Sort output
  #---------------------------------------------------------------------------
  
  sort_columns <- c(
    "datetime",
    group_by
  )
  
  output <- output[
    do.call(
      order,
      output[sort_columns]
    ),
    ,
    drop = FALSE
  ]
  
  rownames(output) <- NULL
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(output)
  
}