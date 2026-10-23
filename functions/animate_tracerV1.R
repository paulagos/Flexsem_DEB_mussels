################################################################################
# 01_animate_tracer_pilot.R
#
# Passive tracer animation
################################################################################

library(FlexsemTools)
library(gifski)
library(cmocean)

#------------------------------------------------------------------------------
# Settings
#------------------------------------------------------------------------------

file <- "C:/AU/farmsIDWNS2026/IDW/2019/Farm_tracers/AsVig_IDW_2019_TRC_rotated.emb"

output_dir <- "Animation_TRC"
frame_dir  <- file.path(output_dir, "frames")

dir.create(output_dir,
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(frame_dir,
           recursive = TRUE,
           showWarnings = FALSE)

fps <- 15
layer <- 5
frame_step <- 5

#------------------------------------------------------------------------------
# Open tracer file
#------------------------------------------------------------------------------

fs <- openFSFile(file)

h <- getHeader(fs)

itemTRC <- which(h$itemNames == "TRC") - 1

#------------------------------------------------------------------------------
# Farm geometry
# (derived from the model XML)
#------------------------------------------------------------------------------

farm_lines <- list(
  
  # Block 1
  c(202,203,204,205),
  c(180,181,182,183),
  c(158,159,160,161),
  c(136,137,138,139),
  c(114,115,116,117),
  c(92,93,94,95),
  
  # Block 2
  c(212,213,214,215),
  c(190,191,192,193),
  c(168,169,170,171),
  c(146,147,148,149),
  c(124,125,126,127),
  c(102,103,104,105)
  
)

#------------------------------------------------------------------------------
# Determine colour limits
#------------------------------------------------------------------------------

cat("Scanning colour scale...\n")

zmin <- Inf
zmax <- -Inf

for (ts in seq(0, h$noTimestep - 1, by = frame_step)) {
  
  trc <- readLayer(
    fs,
    itemTRC,
    ts,
    layer
  )
  
  trc <- sqrt(pmax(trc, 0))
  
  zmin <- min(zmin, min(trc, na.rm = TRUE))
  zmax <- max(zmax, max(trc, na.rm = TRUE))
  
}

cat("Range:", zmin, "-", zmax, "\n")

#------------------------------------------------------------------------------
# Render frames
#------------------------------------------------------------------------------

cat("Rendering...\n")

png_files <- character()

frames <- seq(
  0,
  h$noTimestep - 1,
  by = frame_step
)

for (i in seq_along(frames)) {
  
  ts <- frames[i]
  
  trc <- readLayer(
    fs,
    itemTRC,
    ts,
    layer
  )
  
  trc <- sqrt(pmax(trc, 0))
  
  tm <- h$startDT + ts * h$timestep
  
  png_file <- file.path(
    frame_dir,
    sprintf("frame_%05d.png", i)
  )
  
  png(
    filename = png_file,
    width = 1800,
    height = 1400,
    res = 200
  )
  
  fsAreaPlot(
    h,
    trc,
    elemBorder = NA,
    ColorRamp = cmocean("matter")(256),
    zlim = c(zmin, zmax),
    asp = 1,
    main = NULL
  )
  
  #---------------------------------------------------------------------------
  # Draw farm longlines
  #---------------------------------------------------------------------------
  
  for (line in farm_lines) {
    
    lines(
      h$elemXCoor[line],
      h$elemYCoor[line],
      col = adjustcolor("grey15", alpha.f = 0.70),
      lwd = 2.5,
      lend = "round",
      ljoin = "round"
    )
    
    points(
      h$elemXCoor[line],
      h$elemYCoor[line],
      pch = 16,
      cex = 0.45,
      col = "black"
    )
    
  }
  
  title(
    main = "Passive Tracer",
    sub = format(tm, "%d %b %Y  %H:%M"),
    cex.main = 1.6,
    cex.sub = 1.1
  )
  
  dev.off()
  
  png_files[i] <- png_file
  
  cat(
    "\rFrame",
    i,
    "/",
    length(frames)
  )
  
}

cat("\nDone.\n")

#------------------------------------------------------------------------------
# Create GIF
#------------------------------------------------------------------------------

gifski(
  png_files,
  gif_file = file.path(
    output_dir,
    "Tracer.gif"
  ),
  delay = 1 / fps,
  width = 1800,
  height = 1400
)

cat("\nAnimation complete! 🎉🎉\n")