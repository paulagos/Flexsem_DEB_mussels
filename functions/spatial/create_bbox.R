###############################################################################
# CREATE BOUNDING BOX
###############################################################################

create_bbox <- function(mesh,
                        buffer_x = 0,
                        buffer_y = 0){
  
  bbox <- c(
    xmin = min(mesh$elemXCoor) - buffer_x,
    xmax = max(mesh$elemXCoor) + buffer_x,
    ymin = min(mesh$elemYCoor) - buffer_y,
    ymax = max(mesh$elemYCoor) + buffer_y
  )
  
  return(bbox)
  
}