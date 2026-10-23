################################################################################
# Rotate farm meshes
#
# Reads the parent hydrodynamic model, extracts currents over the mussel rope
# depth, determines the dominant current direction and rotates the standard
# farm mesh.
#
# By Paulo F. Lagos
################################################################################

# ==============================================================================
# LOAD PACKAGES
# ==============================================================================

library(FlexsemTools)

source("C:/AU/R/emfunc.r")

# ==============================================================================
# USER SETTINGS
# ==============================================================================

# Farm orientation relative to dominant current
# "along"  = current flows parallel to the rows
# "across" = current flows through the rows
farm_orientation <- "along"

# Project folder
project_folder <- "C:/AU/farmsIDWNS2026/IDW/2019"

# Hydrodynamic model
hydro_file <- file.path(
  project_folder,
  "IDW3DUV.emb"
)

# Base mesh
mesh_file <- file.path(
  project_folder,
  "musselFarmV3.mesh"
)

# Output folder
output_folder <- file.path(
  project_folder,
  "Rotated_meshes"
)

# Mussel rope depth (m)
rope_top <- 0.5
rope_bottom <- 3.0

# Direction bin width
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
cat("Rotating farm meshes\n")
cat("----------------------------------------\n\n")

# ==============================================================================
# OPEN MODEL
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

for (i in seq_len(nrow(farms))) {
  
  e <- pointElem(
    mod,
    farms$lon[i],
    farms$lat[i]
  )
  
  if (e < 0) {
    
    e <- pointElemNearest(
      mod,
      farms$lon[i],
      farms$lat[i],
      1
    )
    
  }
  
  farms$element[i] <- e
  
}

# Grenaa point falls on land.
# Use the predefined offshore parent element.
farms$element[farms$name == "Grenaa"] <- 1856

# ==============================================================================
# READ BASE MESH
# ==============================================================================

mesh_base <- readMesh(mesh_file)

# ==============================================================================
# SUMMARY
# ==============================================================================

rotation_summary <- data.frame()

# ==============================================================================
# LOOP THROUGH FARMS
# ==============================================================================
print(farms)

for (i in seq_len(nrow(farms))) {
  
  location <- farms$name[i]
  element <- farms$element[i]
  
  cat("----------------------------------------\n")
  cat(location, "\n")
  
  u <- numeric(n_steps)
  v <- numeric(n_steps)
  
  # ---------------------------------------------------------------------------
  # Read currents
  # ---------------------------------------------------------------------------
  
  for (ts in 0:(n_steps-1)) {
    
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
    
    if(length(idx) == 0){
      
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
  # Current direction
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
  
  dominant_direction <- as.numeric(
    names(
      sort(
        table(direction_bins),
        decreasing = TRUE
      )[1]
    )
  )
  
  u_mean <- mean(u, na.rm = TRUE)
  
  v_mean <- mean(v, na.rm = TRUE)
  
  mean_direction <- (
    atan2(
      u_mean,
      v_mean
    ) * 180 / pi
  ) %% 360
  
  if (farm_orientation == "along") {
    
    rotation_angle <- dominant_direction
    
  } else {
    
    rotation_angle <- (dominant_direction + 90) %% 360
    
  }
  # ---------------------------------------------------------------------------
  # Rotate mesh
  # ---------------------------------------------------------------------------
  
  mesh_rotated <- rotateMesh(
    mesh_base,
    a = rotation_angle
  )
  
  mesh_name <- paste0(
    location,
    ".mesh"
  )
  
  saveMesh(
    mesh_rotated,
    file.path(
      output_folder,
      mesh_name
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console output
  # ---------------------------------------------------------------------------
  
  #cat("Element          :", element, "\n")
  #cat("Direction        :", dominant_direction, "°\n")
  #cat("Rotation         :", rotation_angle, "°\n")
  #cat("Saved            :", mesh_name, "\n\n")
  
  cat("Element          :", element, "\n")
  cat("Modal direction  :", dominant_direction, "°\n")
  cat("Mean direction   :", round(mean_direction,1), "°\n")
  cat("Farm orientation :", farm_orientation, "\n")
  cat("Mesh rotation    :", rotation_angle, "°\n")
  
  # ---------------------------------------------------------------------------
  # Summary
  # ---------------------------------------------------------------------------
  
  rotation_summary <- rbind(
    
    rotation_summary,
    
    data.frame(
      
      Location = location,
      Element = element,
      ModalDirection = dominant_direction,
      MeanDirection = round(mean_direction,1),
      Rotation = rotation_angle,
      Mesh = mesh_name,
      
      stringsAsFactors = FALSE
      
    )
    
  )
  
}

# ==============================================================================
# SAVE SUMMARY
# ==============================================================================

write.csv(
  
  rotation_summary,
  
  file.path(
    output_folder,
    "rotation_summary.csv"
  ),
  
  row.names = FALSE
  
)

# ==============================================================================
# FINISH
# ==============================================================================

cat("----------------------------------------\n")
cat("Finished\n")
cat("----------------------------------------\n\n")

cat("Rotated meshes saved to:\n")
cat(output_folder, "\n")