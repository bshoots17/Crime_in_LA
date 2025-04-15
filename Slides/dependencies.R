library(grid)
library(RSQLite)
library(dplyr)
library(shiny)

# killer plot pipeline functions
standardize_scale <- function(column) {
  breaks <- quantile(column, probs = seq(0, 1, 1/3))
  cuts <- cut(column, breaks = c(-Inf, breaks, Inf))
  lbls <- c(
    paste0("< ", round(breaks[1])),
    paste0("[", round(breaks[1]), ", ", round(breaks[2]), "]"),
    paste0("[", round(breaks[2]), ", ", round(breaks[3]), "]"),
    paste0("[", round(breaks[3]), ", ", round(breaks[4]), "]"),
    paste0("> ", round(breaks[4]))
  )
  return(factor(cuts, levels = levels(cuts), labels = lbls))
}

generate_star_centers <- function(n_cols, n_rows, radius = 0.05 / 2, padding = c(0.05, .1)) {
  # X spacing
  usable_width <- 1 - 2 * padding[1]
  step_x <- (usable_width - 2 * radius) / (n_cols - 1)
  spaceX <- step_x - 2 * radius
  x_start <- padding[1]
  x <- seq(x_start, by = (2 * radius + spaceX), length.out = n_cols)
  
  # Y spacing with top and bottom padding
  usable_height <- 1 - 2 * padding[2] - .1
  
  # Handle the case where n_rows is 1 to avoid NaN (division by zero)
  if (n_rows > 1) {
    step_y <- (usable_height - 2 * radius) / (n_rows - 1)
    spaceY <- step_y - 2 * radius
  } else {
    step_y <- usable_height
    spaceY <- 0
  }
  
  y_start <- ifelse(n_rows > 1, padding[2] + .125, .5)
  y <- seq(y_start, by = (2 * radius + spaceY), length.out = n_rows)
  
  # Create grid of star centers
  stars <- expand.grid(x = x, y = y)
  
  # Track rows and columns to resort by row
  stars$row <- rep(1:n_rows, times = n_cols)
  stars$col <- rep(1:n_cols, each = n_rows)
  stars <- stars[order(stars$col, stars$row), ]
  
  return(stars)
}
create_star <- function(scale_factor = 1, center = c(0.06, 0.06), fill = '#a44c5c', color = "#d3af37") {
  # based on this function: https://eng.libretexts.org/Courses/Oxnard_College/Matlab_and_Octave_Programming_for_STEM_Applications_%28Smith%29/05%3A_User-Defined_Functions/5.08%3A_Matlab_Functions_with_No_Outputs 
  r_outer <- 0.01 * scale_factor
  r_inner <- r_outer / 2.5
  
  degrees <- pi / 5 # 5-pointed star 
  
  rotation <- pi / 2 # 90 degrees, rotates
  
  theta <- rotation + 0:9 * degrees
  r <- rep(c(r_outer, r_inner), length.out = 10)
  
  # create the star coordinates
  x <- center[1] + r * cos(theta)
  y <- center[2] + r * sin(theta) * 1.5  # x1.5 because it is squished on the y-axis
  
  coords <- list('x' = c(x, x[1]), 'y' = c(y, y[1]))
  return(polygonGrob(x = coords$x, y = coords$y, gp = gpar(fill = fill, col = color)))  # add the first point at the end to complete the shape
}