###############################################################################
# FLEXSEM DEB MUSSEL MODEL
# Script 02 - Read DEB Outputs v2.1.0
#
# Description:
# Reads all FlexSem DEB model outputs discovered by
# 01_get_config_DEBV2.R.
#
# Variables are detected automatically from each .emb file.
#
# For spatial DEB outputs, the reader:
#
#   1. Uses the active 3D farm definition
#   2. Applies the user-defined analysis scope
#   3. Reads all selected Element × Layer cells
#   4. Aggregates each variable immediately
#   5. Preserves the requested spatial dimensions
#
# Sediment outputs are temporarily handled with the original
# single Element × Layer extraction method.
#
# Output:
#
#   DEB_outputs
#
# Structure:
#
# DEB_outputs
#   ├── metadata
#   ├── deb_fluxes
#   ├── deb_mussels
#   ├── deb_state_variables
#   ├── environment
#   ├── mussels_farm
#   └── sediment
#
# Each output contains one dataframe per model location.
#
# Author: Paulo F. Lagos
###############################################################################

###############################################################################
# LOAD PACKAGES
###############################################################################

library(dplyr)

###############################################################################
# CHECK REQUIRED OBJECTS
###############################################################################
required_objects <- c(
  "emb_files",
  "master_folder",
  "layers",
  "elem",
  "farm_cells",
  "analysis_scope"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    function(obj) {
      exists(
        obj,
        inherits = TRUE
      )
    },
    logical(1)
  )
]

if (length(missing_objects) > 0) {
  
  stop(
    paste(
      "Required objects are missing:",
      paste(missing_objects, collapse = ", "),
      "\nRun the toolbox setup steps before 02_get_DEBV2.R."
    )
  )
  
}


###############################################################################
# HELPER FUNCTIONS
###############################################################################

#------------------------------------------------------------------------------
# Return FlexSem variable index
#------------------------------------------------------------------------------

get_var_index <- function(header,
                          variable) {
  
  idx <- match(
    variable,
    header$itemNames
  )
  
  if (is.na(idx)) {
    
    stop(
      paste(
        "Variable not found:",
        variable
      )
    )
    
  }
  
  # FlexSem uses zero-based item numbering
  
  idx - 1
  
}


#------------------------------------------------------------------------------
# Read one standard single-cell FlexSem output file
#
# Used temporarily for sediment outputs.
#------------------------------------------------------------------------------

read_emb_file_single <- function(file,
                                 elem,
                                 layer) {
  
  fs <- openFSFile(file)
  
  header <- getHeader(fs)
  
  variables <- header$itemNames
  
  cat(
    "      Variables detected :",
    length(variables),
    "\n"
  )
  
  df <- NULL
  
  for (variable in variables) {
    
    idx <- get_var_index(
      header,
      variable
    )
    
    ts <- fsReadTimeseries(
      fs,
      idx,
      elem,
      layer
    )
    
    if (is.null(df)) {
      
      df <- data.frame(
        
        datetime = ts[, 1],
        
        stringsAsFactors = FALSE
        
      )
      
    }
    
    df[[variable]] <- ts[, 2]
    
  }
  
  return(df)
  
}


#------------------------------------------------------------------------------
# Read one spatial FlexSem output file
#
# Each variable is:
#
#   1. Read across selected farm cells
#   2. Aggregated immediately
#   3. Added to the final dataframe
#
# This avoids storing all variables × all cells × all timesteps simultaneously.
#------------------------------------------------------------------------------

read_emb_file_spatial <- function(file,
                                  selected_cells,
                                  analysis_scope) {
  
  #---------------------------------------------------------------------------
  # Open file and inspect header
  #---------------------------------------------------------------------------
  
  fs <- openFSFile(file)
  
  header <- getHeader(fs)
  
  variables <- header$itemNames
  
  cat(
    "      Variables detected :",
    length(variables),
    "\n"
  )
  
  cat(
    "      Selected 3D cells  :",
    nrow(selected_cells),
    "\n"
  )
  
  cat(
    "      Aggregation        :",
    analysis_scope$aggregation,
    "\n"
  )
  
  cat(
    "      Preserve           :",
    if (is.null(analysis_scope$preserve)) {
      "NONE"
    } else {
      paste(
        analysis_scope$preserve,
        collapse = ", "
      )
    },
    "\n"
  )
  
  #---------------------------------------------------------------------------
  # Final output dataframe
  #---------------------------------------------------------------------------
  
  df <- NULL
  
  #---------------------------------------------------------------------------
  # Loop through variables
  #---------------------------------------------------------------------------
  
  for (variable in variables) {
    
    cat(
      "         Reading ",
      variable,
      "\n",
      sep = ""
    )

    #-------------------------------------------------------------------------
    # Read selected cells
    #
    # Two reading paths are available:
    #
    #   aggregation = "none"
    #     → preserve every selected 3D cell
    #     → use long-format spatial reader
    #
    #   aggregation != "none"
    #     → aggregate while reading
    #     → avoid creating a very large intermediate dataframe
    #-------------------------------------------------------------------------
    
    if (analysis_scope$aggregation == "none") {
      
      variable_data <- read_selected_cells(
        
        file = file,
        
        variable = variable,
        
        selected_cells = selected_cells
        
      )
      
    } else {
      
      variable_data <- read_aggregated_cells(
        
        file = file,
        
        variable = variable,
        
        selected_cells = selected_cells,
        
        method = analysis_scope$aggregation,
        
        group_by = analysis_scope$preserve
        
      )
      
    }
    #-------------------------------------------------------------------------
    # Rename value column to variable name
    #-------------------------------------------------------------------------
    
    names(variable_data)[
      names(variable_data) == "value"
    ] <- variable
    
    #-------------------------------------------------------------------------
    # Add variable to final dataframe
    #-------------------------------------------------------------------------
    
    if (is.null(df)) {
      
      df <- variable_data
      
    } else {
      
      join_columns <- intersect(
        
        names(df),
        
        c(
          "datetime",
          "Mask",
          "Element",
          "Layer",
          "Depth"
        )
        
      )
      
      df <- dplyr::left_join(
        
        df,
        
        variable_data,
        
        by = join_columns
        
      )
      
    }
    
    #-------------------------------------------------------------------------
    # Remove temporary variable data
    #-------------------------------------------------------------------------
    
    rm(variable_data)
    
  }
  
  #---------------------------------------------------------------------------
  # Sort final dataframe
  #---------------------------------------------------------------------------
  
  sort_columns <- intersect(
    
    c(
      "datetime",
      "Mask",
      "Element",
      "Layer",
      "Depth"
    ),
    
    names(df)
    
  )
  
  df <- df |>
    
    dplyr::arrange(
      dplyr::across(
        dplyr::all_of(sort_columns)
      )
    )
  
  #---------------------------------------------------------------------------
  # Return
  #---------------------------------------------------------------------------
  
  return(
    as.data.frame(df)
  )
  
}


###############################################################################
# SELECT ACTIVE FARM CELLS
###############################################################################

selected_cells <- select_farm_cells(
  
  farm_cells = farm_cells,
  
  masks = analysis_scope$masks,
  
  elements = analysis_scope$elements,
  
  layers = analysis_scope$layers,
  
  depths = analysis_scope$depths
  
)

cat("\n")
cat(
  "Selected active 3D cells :",
  nrow(selected_cells),
  "\n"
)

cat(
  "Selected masks           :",
  paste(
    unique(selected_cells$Mask),
    collapse = ", "
  ),
  "\n"
)

cat(
  "Selected elements        :",
  length(unique(selected_cells$Element)),
  "\n"
)

cat(
  "Selected layers          :",
  paste(
    sort(unique(selected_cells$Layer)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "Selected depths          :",
  paste(
    sort(unique(selected_cells$Depth)),
    collapse = ", "
  ),
  "m\n"
)


###############################################################################
# READ ALL FLEXSEM OUTPUTS
###############################################################################

cat("\n")
cat("=========================================================\n")
cat("READING FLEXSEM OUTPUT FILES\n")
cat("=========================================================\n")


###############################################################################
# MAIN OUTPUT OBJECT
###############################################################################

DEB_outputs <- list()


###############################################################################
# METADATA
###############################################################################

DEB_outputs$metadata <- list(
  
  analysis_scope = analysis_scope,
  
  selected_cells = selected_cells,
  
  number_selected_cells = nrow(selected_cells),
  
  extraction_element_sediment = elem,
  
  extraction_layers = layers,
  
  read_date = Sys.time()
  
)


###############################################################################
# LOOP THROUGH EACH OUTPUT TYPE
###############################################################################

for (output_name in names(emb_files)) {
  
  cat("\n")
  cat("=========================================================\n")
  cat("Output file :", output_name, "\n")
  cat("=========================================================\n")
  
  # Create sub-list for this output type
  
  DEB_outputs[[output_name]] <- list()
  
  files <- emb_files[[output_name]]
  
  
  #===========================================================================
  # LOOP THROUGH LOCATIONS
  #===========================================================================
  
  for (file in files) {
    
    #-------------------------------------------------------------------------
    # Extract location name from folder structure
    #
    # .../Output_DEB_AsVig/results_xxx/deb_fluxes.emb
    #-------------------------------------------------------------------------
    
    location <- basename(
      dirname(
        dirname(file)
      )
    )
    
    location <- sub(
      "^Output_DEB_",
      "",
      location
    )
    
    cat("\n")
    cat("  Location :", location, "\n")
    
    
    #=========================================================================
    # SEDIMENT
    #
    # Temporary original single-cell extraction.
    #=========================================================================
    
    if (output_name == "sediment") {
      
      current_layer <- layers[[output_name]]
      
      if (is.null(current_layer)) {
        
        stop(
          paste(
            "No extraction layer defined for output:",
            output_name
          )
        )
        
      }
      
      cat("      Reading mode       : single cell\n")
      cat("      Element            :", elem, "\n")
      cat("      Layer              :", current_layer, "\n")
      
      df <- read_emb_file_single(
        
        file = file,
        
        elem = elem,
        
        layer = current_layer
        
      )
      
      
      #========================================================================
      # ALL OTHER OUTPUTS
      #
      # Multidimensional farm extraction.
      #========================================================================
      
    } else {
      
      cat("      Reading mode       : spatial\n")
      
      df <- read_emb_file_spatial(
        
        file = file,
        
        selected_cells = selected_cells,
        
        analysis_scope = analysis_scope
        
      )
      
    }
    
    
    #-------------------------------------------------------------------------
    # Store dataframe
    #-------------------------------------------------------------------------
    
    DEB_outputs[[output_name]][[location]] <- df
    
    
    #-------------------------------------------------------------------------
    # Report result
    #-------------------------------------------------------------------------
    
    dimension_columns <- intersect(
      
      c(
        "Mask",
        "Element",
        "Layer",
        "Depth"
      ),
      
      names(df)
      
    )
    
    variable_columns <- setdiff(
      
      names(df),
      
      c(
        "datetime",
        dimension_columns
      )
      
    )
    
    cat(
      "      Output rows        :",
      nrow(df),
      "\n"
    )
    
    cat(
      "      Variables          :",
      length(variable_columns),
      "\n"
    )
    
    if (length(dimension_columns) > 0) {
      
      cat(
        "      Preserved dimensions:",
        paste(
          dimension_columns,
          collapse = ", "
        ),
        "\n"
      )
      
    }
    
    cat("      ✓ Finished\n")
    
  }
  
}


###############################################################################
# SAVE OUTPUTS
###############################################################################

cat("\n")
cat("=========================================================\n")
cat("SAVING OUTPUTS\n")
cat("=========================================================\n")

output_dir <- file.path(
  master_folder,
  "R_objects"
)

if (!dir.exists(output_dir)) {
  
  dir.create(
    output_dir,
    recursive = TRUE
  )
  
}


###############################################################################
# SAVE METADATA
###############################################################################

saveRDS(
  
  DEB_outputs$metadata,
  
  file.path(
    output_dir,
    "metadata.rds"
  )
  
)


###############################################################################
# SAVE EACH OUTPUT SEPARATELY
###############################################################################

for (output_name in names(emb_files)) {
  
  saveRDS(
    
    DEB_outputs[[output_name]],
    
    file.path(
      
      output_dir,
      
      paste0(
        output_name,
        ".rds"
      )
      
    )
    
  )
  
  cat(
    sprintf(
      "Saved %-22s\n",
      paste0(
        output_name,
        ".rds"
      )
    )
  )
  
}


###############################################################################
# SUMMARY
###############################################################################

cat("\n")
cat("=========================================================\n")
cat("SUMMARY\n")
cat("=========================================================\n\n")

for (output_name in names(emb_files)) {
  
  output_list <- DEB_outputs[[output_name]]
  
  if (length(output_list) == 0) {
    next
  }
  
  first_location <- output_list[[1]]
  
  dimension_columns <- intersect(
    
    c(
      "Mask",
      "Element",
      "Layer",
      "Depth"
    ),
    
    names(first_location)
    
  )
  
  variable_columns <- setdiff(
    
    names(first_location),
    
    c(
      "datetime",
      dimension_columns
    )
    
  )
  
  cat("-----------------------------------------\n")
  cat(output_name, "\n")
  cat("-----------------------------------------\n")
  
  cat(
    "Locations :",
    length(output_list),
    "\n"
  )
  
  cat(
    "Variables :",
    length(variable_columns),
    "\n"
  )
  
  cat(
    "Rows      :",
    nrow(first_location),
    "\n"
  )
  
  if (length(dimension_columns) > 0) {
    
    cat(
      "Dimensions:",
      paste(
        dimension_columns,
        collapse = ", "
      ),
      "\n"
    )
    
  }
  
  cat("Location names:\n")
  
  print(
    names(output_list)
  )
  
  cat("\n")
  
}


###############################################################################
# FINISH
###############################################################################

cat("=========================================================\n")
cat("READING COMPLETED SUCCESSFULLY\n")
cat("=========================================================\n")