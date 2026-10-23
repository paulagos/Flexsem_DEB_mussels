###############################################################################
# MESH TO POLYGONS
###############################################################################

mesh_to_polygons <- function(mesh,
                             crs = 25832){
  
  # Node coordinates
  nodes <- data.frame(
    x = mesh$nodeXCoor,
    y = mesh$nodeYCoor
  )
  
  # Element connectivity (R uses 1-based indexing)
  connectivity <- lapply(mesh$elemNodes, function(x) x + 1)
  
  # Create polygons
  polygons <- lapply(connectivity, function(idx){
    
    coords <- nodes[idx, ]
    
    # Close polygon
    coords <- rbind(coords, coords[1, ])
    
    sf::st_polygon(list(as.matrix(coords)))
    
  })
  
  # Convert to sf object
  polygons_sf <- sf::st_sf(
    
    elem_id = seq_along(polygons),
    
    elem_x = mesh$elemXCoor,
    
    elem_y = mesh$elemYCoor,
    
    geometry = sf::st_sfc(polygons, crs = crs)
    
  )
  
  return(polygons_sf)
  
}