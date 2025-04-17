# Load libraries
library(httr)
library(jsonlite)
library(rgl)
options(rgl.useNULL = FALSE)

# 1. Fetch real wind data (Miami)
res <- GET("https://api.open-meteo.com/v1/forecast",
           query = list(
             latitude = 25.76,
             longitude = -80.19,
             hourly = "windspeed_10m",
             start = Sys.Date(),
             end = Sys.Date() + 1
           ))

windspeed <- fromJSON(content(res, as = "text"))$hourly$windspeed_10m
avg_wind <- mean(windspeed, na.rm = TRUE) / 10

# 2. Define vector field
generate_vector <- function(x, y, z) {
  u <- -y
  v <-  x
  w <- avg_wind
  norm <- sqrt(u^2 + v^2 + w^2)
  c(u, v, w) / norm
}

# 3. Compute streamline
compute_streamline <- function(p0, steps = 80, step_size = 0.3) {
  path <- matrix(NA, nrow = steps, ncol = 3)
  path[1, ] <- p0
  for (i in 2:steps) {
    v <- generate_vector(path[i - 1, 1], path[i - 1, 2], path[i - 1, 3])
    path[i, ] <- path[i - 1, ] + step_size * v
  }
  path
}

# 4. Seed points
seeds <- expand.grid(x = seq(-3, 3, length.out = 5),
                     y = seq(-3, 3, length.out = 5))
seeds$z <- 0

# 5. Function to render and save from different angles
render_scene <- function(filename, theta, phi, zoom = 0.9) {
  open3d(windowRect = c(100, 100, 2400, 1800))
  bg3d("white")
  view3d(theta = theta, phi = phi, zoom = zoom)
  
  # Streamlines
  for (i in 1:nrow(seeds)) {
    path <- compute_streamline(as.numeric(seeds[i, ]))
    lines3d(path, col = "#8E44AD", lwd = 2)
  }
  
  # Axes with thicker lines and enlarged tick labels
  axes3d(edges = "x++", col = "#D55E00", nticks = 5, lwd = 3, cex = 2)
  axes3d(edges = "y++", col = "#009E73", nticks = 5, lwd = 3, cex = 2)
  axes3d(edges = "z++", col = "#0072B2", nticks = 5, lwd = 3, cex = 2)
  
  # Legend with larger font
  legend3d("topright", legend = c("X Axis (Orange)", "Y Axis (Green)", "Z Axis (Blue)"),
           fill = c("#D55E00", "#009E73", "#0072B2"), cex = 2.4, bty = "n")
  
  Sys.sleep(0.1)
  rgl.snapshot(filename)
  close3d()
}

# 6. Save from multiple diverse angles
render_scene("vortex_view1.png", theta = 35, phi = 25)
render_scene("vortex_view2.png", theta = 180, phi = 40)
render_scene("vortex_view3.png", theta = 90, phi = 10)
