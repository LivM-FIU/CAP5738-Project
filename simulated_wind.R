library(ggplot2)
library(dplyr)
library(viridis)

# Creating a grid of points
grid_size <- 20
x <- seq(-pi, pi, length.out = grid_size)
y <- seq(-pi, pi, length.out = grid_size)
grid <- expand.grid(x = x, y = y)

# Defining vector components for a rotational wind pattern
grid <- grid %>%
  mutate(
    u = -y,        # east-west component (longitude)
    v = x,         # north-south component (latitude)
    speed = sqrt(u^2 + v^2)  # vector magnitude
  )

# Plot using ggplot2
ggplot(grid, aes(x = x, y = y)) +
  geom_segment(aes(xend = x + u * 0.1, yend = y + v * 0.1, color = speed),
               arrow = arrow(length = unit(0.15, "cm")),
               lineend = "round", size = 0.7) +
  scale_color_viridis(name = "Speed", option = "D") +
  labs(
    title = "Simulated Wind Flow Over a Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 14)
