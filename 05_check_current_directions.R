################################################################################
# Check current directions for farm orientation
#
# Reads the parent hydrodynamic model, extracts currents over the mussel rope
# depth and compares the modal current direction with the mean vector direction.
#
# Creates current rose plots for each farm.
#
# By Paulo F. Lagos
################################################################################

# ==============================================================================
# LOAD PACKAGES
# ==============================================================================

library(FlexsemTools)
library(ggplot2)
library(dplyr)
library(patchwork)
library(stringr)

source("C:/AU/R/emfunc.r")

# ==============================================================================
# USER SETTINGS
# ==============================================================================

project_folder <- "C:/AU/farmsIDWNS2026/IDW/2019"

hydro_file <- file.path(
  project_folder,
  "IDW3DUV.emb"
)

output_folder <- file.path(
  project_folder,
  "Ocean_currents_diagnostics"
)

rope_top <- 0.5
rope_bottom <- 3.0

bin_width <- 15

# ==============================================================================
# FARM LOCATIONS
# ==============================================================================

farms <- data.frame(
  
  name = c(
    "AsVig",
    "Grenaa",
    "Kerteminde",
    "Anholt",
    "Hesselo",
    "Samso",
    "Kattegat"
  ),
  
  lon = c(
    10.07,
    10.56,
    10.80,
    11.23,
    11.69,
    10.43,
    11.95
  ),
  
  lat = c(
    55.78,
    56.29,
    55.45,
    56.60,
    56.35,
    56.00,
    56.74
  ),
  
  stringsAsFactors = FALSE
  
)

# ==============================================================================
# CREATE OUTPUT FOLDER
# ==============================================================================

if (!dir.exists(output_folder))
  dir.create(output_folder, recursive = TRUE)

# ==============================================================================
# START
# ==============================================================================

cat("\n")
cat("----------------------------------------\n")
cat("Checking current directions\n")
cat("----------------------------------------\n\n")

# ==============================================================================
# OPEN HYDRODYNAMIC MODEL
# ==============================================================================

mod <- openFSFile(hydro_file)

header <- getHeader(mod)

u_idx <- which(header$itemNames == "u") - 1
v_idx <- which(header$itemNames == "v") - 1

n_steps <- header$noTimestep

# ==============================================================================
# FIND FARM ELEMENTS
# ==============================================================================

farms$element <- integer(nrow(farms))

for(i in seq_len(nrow(farms))){
  
  e <- pointElem(
    mod,
    farms$lon[i],
    farms$lat[i]
  )
  
  if(e < 0){
    
    e <- pointElemNearest(
      mod,
      farms$lon[i],
      farms$lat[i],
      1
    )
    
  }
  
  farms$element[i] <- e
  
}

# Grenaa parent element
farms$element[farms$name == "Grenaa"] <- 1856

print(farms)

# ==============================================================================
# STORAGE
# ==============================================================================

plot_list <- list()

summary_df <- data.frame()

# ==============================================================================
# LOOP THROUGH FARMS
# ==============================================================================

for(i in seq_len(nrow(farms))){
  
  location <- farms$name[i]
  element  <- farms$element[i]
  
  cat("----------------------------------------\n")
  cat(location,"\n")
  
  u <- numeric(n_steps)
  v <- numeric(n_steps)
  
  # ---------------------------------------------------------------------------
  # READ CURRENTS
  # ---------------------------------------------------------------------------
  
  for(ts in 0:(n_steps-1)){
    
    u_profile <- readProfile(
      mod,
      u_idx,
      ts,
      element
    )
    
    v_profile <- readProfile(
      mod,
      v_idx,
      ts,
      element
    )
    
    idx <- which(
      
      u_profile$depth >= rope_top &
        u_profile$depth <= rope_bottom
      
    )
    
    if(length(idx)==0){
      
      idx <- which.min(
        
        abs(
          
          u_profile$depth -
            (rope_top + rope_bottom)/2
          
        )
        
      )
      
    }
    
    u[ts+1] <- mean(
      u_profile$value[idx],
      na.rm = TRUE
    )
    
    v[ts+1] <- mean(
      v_profile$value[idx],
      na.rm = TRUE
    )
    
  }
  
  # ---------------------------------------------------------------------------
  # CALCULATE DIRECTIONS
  # ---------------------------------------------------------------------------
  
  direction <- (
    
    atan2(
      u,
      v
    ) * 180 / pi
    
  ) %% 360
  
  direction_bins <- floor(
    direction / bin_width
  ) * bin_width
  
  modal_direction <- as.numeric(
    
    names(
      
      sort(
        table(direction_bins),
        decreasing = TRUE
      )[1]
      
    )
    
  )
  
  u_mean <- mean(
    u,
    na.rm = TRUE
  )
  
  v_mean <- mean(
    v,
    na.rm = TRUE
  )
  
  mean_direction <- (
    
    atan2(
      u_mean,
      v_mean
    ) * 180 / pi
    
  ) %% 360
  
  # ---------------------------------------------------------------------------
  # ROSE DATA
  # ---------------------------------------------------------------------------
  
  rose_df <- data.frame(
    direction = direction_bins
  ) |>
    
    count(direction)
  
  
  # ---------------------------------------------------------------------------
  # CREATE ROSE PLOT
  # ---------------------------------------------------------------------------
  
  max_count <- max(rose_df$n)
  
  modal_df <- data.frame(
    direction = modal_direction,
    n = max_count
  )
  
  mean_df <- data.frame(
    direction = mean_direction,
    n = max_count
  )
  
  p <- ggplot(
    rose_df,
    aes(
      x = direction,
      y = n
    )
  ) +
    
    geom_col(
      width = bin_width,
      fill = "steelblue",
      colour = "black",
      alpha = 0.85
    ) +
    
    # Modal direction (blue)
    geom_col(
      data = modal_df,
      aes(
        x = direction,
        y = n
      ),
      width = 1,
      fill = "blue"
    ) +
     
    # Mean direction (red)
    geom_col(
      data = mean_df,
      aes(
        x = direction,
        y = n
      ),
      width = 1,
      fill = "red"
    ) +
    
    coord_polar(
      start = -pi/24
    ) +
    
    scale_x_continuous(
      
      breaks = seq(
        0,
        315,
        by = 45
      ),
      
      labels = c(
        "N",
        "NE",
        "E",
        "SE",
        "S",
        "SW",
        "W",
        "NW"
      )
      
    ) +
    
    labs(
      
      title = location,
      
      subtitle = paste0(
        "Modal = ",
        modal_direction,
        "°   |   Mean = ",
        round(mean_direction,1),
        "°"
      ),
      
      x = NULL,
      y = NULL
      
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 10
      ),
      
      axis.text.y = element_blank(),
      
      axis.ticks = element_blank(),
      
      panel.grid.major = element_line(
        colour = "grey80"
      )
      
    )
  
  # ---------------------------------------------------------------------------
  # SAVE INDIVIDUAL FIGURE
  # ---------------------------------------------------------------------------
  
  ggsave(
    
    filename = file.path(
      
      output_folder,
      
      paste0(
        location,
        "_current_rose.png"
      )
      
    ),
    
    plot = p,
    
    width = 5,
    height = 5,
    dpi = 600
    
  )
  
  # ---------------------------------------------------------------------------
  # STORE PLOT
  # ---------------------------------------------------------------------------
  
  plot_list[[location]] <- p
  
  # ---------------------------------------------------------------------------
  # CONSOLE OUTPUT
  # ---------------------------------------------------------------------------
  
  cat("Element          :", element, "\n")
  cat("Modal direction  :", modal_direction, "°\n")
  cat("Mean direction   :", round(mean_direction,1), "°\n\n")
  
  # ---------------------------------------------------------------------------
  # SUMMARY TABLE
  # ---------------------------------------------------------------------------
  
  summary_df <- rbind(
    
    summary_df,
    
    data.frame(
      
      Location = location,
      
      Element = element,
      
      ModalDirection = modal_direction,
      
      MeanDirection = round(
        mean_direction,
        1
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  )
  
}

# ==============================================================================
# COMBINED FIGURE
# ==============================================================================

combined_plot <- wrap_plots(
  
  plot_list,
  
  ncol = 3
  
)

ggsave(
  
  filename = file.path(
    
    output_folder,
    
    "Combined_current_roses.png"
    
  ),
  
  plot = combined_plot,
  
  width = 12,
  
  height = 10,
  
  dpi = 600
  
)

# ==============================================================================
# SAVE SUMMARY
# ==============================================================================

write.csv(
  
  summary_df,
  
  file.path(
    
    output_folder,
    
    "Current_summary.csv"
    
  ),
  
  row.names = FALSE
  
)

# ==============================================================================
# FINISH
# ==============================================================================

cat("----------------------------------------\n")
cat("Finished\n")
cat("----------------------------------------\n\n")

cat("Figures saved to:\n")
cat(output_folder,"\n")