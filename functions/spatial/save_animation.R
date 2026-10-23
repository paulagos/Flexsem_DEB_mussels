###############################################################################
# SAVE ANIMATION
###############################################################################

save_animation <- function(animation,
                           filename){
  
  gganimate::anim_save(
    filename = filename,
    animation = animation
  )
  
}