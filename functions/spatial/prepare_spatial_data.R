###############################################################################
# PREPARE SPATIAL DATA
###############################################################################

prepare_spatial_data <- function(project){
  
  # Read mesh
  mesh <- fsReadMesh(prohect$mesh_file)
  
  # Create polygons
  polygons <- mesh_to_polygons(mesh)
  
  # Read coastline
  coast <- sf::st_read(config$coastline_file, quiet = TRUE)
  
  # Bounding box
  bbox <- c(
    min(mesh$elemXCoor),
    max(mesh$elemXCoor),
    min(mesh$elemYCoor),
    max(mesh$elemYCoor)
  )
  
  # Return
  return(list(
    mesh      = mesh,
    polygons  = polygons,
    coastline = coast,
    bbox      = bbox
  ))
  
}