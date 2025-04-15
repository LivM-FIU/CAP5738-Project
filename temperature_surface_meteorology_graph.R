# The graph is a static 3D surface plot created using surf3D().
# It visualizes a scalar field as a smooth surface embedded in 3D space.
# Each point on the surface corresponds to a scalar value at coordinates (x,y)
# with the surface height (z-axis) representing the scalar magnitude. 
#A color gradient (from the viridis colormap) is used to reinforce scalar variations visually.

library(plot3D)
library(viridis)

# Generate data
x <- seq(-10, 10, length.out = 100)
y <- seq(-10, 10, length.out = 100)
X <- matrix(rep(x, each = 100), nrow = 100)
Y <- matrix(rep(y, times = 100), nrow = 100)
Z <- 30 - (Y^2 / 20) - abs(5 * cos(X / 3) * sin(Y / 3))

# Output image
png("temperature_surface_meteorology_final.png", width = 1600, height = 1200, res = 120)

# Create plot
surf3D(
  x = X, y = Y, z = Z,
  colvar = Z,
  col = viridis(100),
  bty = "b2",               # full 3D box
  ticktype = "detailed",    # helps spacing
  theta = 40, phi = 25,
  xlab = "Longitude",
  ylab = "Latitude",
  zlab = "Temperature (°C)",
  main = "3D Temperature Field Over Geographic Region",
  cex.lab = 1.8,           # label size
  cex.axis = 1.2,
  cex.main = 2,
  shade = 0.5,
  lighting = TRUE,
  ltheta = 135
)

dev.off()
