# Load libraries
library(tidyverse)
library(scatterplot3d)

# Load and prepare earthquake data
url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
df <- read_csv(url, show_col_types = FALSE) %>%
  select(latitude, longitude, depth, mag, place) %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(depth), !is.na(mag)) %>%
  filter(mag >= 2.5) %>%
  mutate(
    norm_mag = (mag - min(mag)) / diff(range(mag)),
    color_index = cut(norm_mag, breaks = 100, labels = FALSE),
    color = colorRampPalette(c("blue", "green", "yellow", "red"))(100)[color_index]
  )

# ✅ Save the plot as a PNG image
png("earthquake_3d_plot_with_legend.png", width = 1200, height = 1000, res = 150)

# Create the 3D scatter plot
s3d <- scatterplot3d(
  x = df$longitude,
  y = df$latitude,
  z = -df$depth,
  color = df$color,
  pch = 20,
  main = "Earthquake Locations (Past 30 Days)",
  xlab = "Longitude",
  ylab = "Latitude",
  zlab = "Depth (km)",
  angle = 45,
  box = TRUE,
  grid = TRUE
)

# Add legend in the right side with proper sizing
legend("right", inset = c(0.01, 0.01),
       title = "Magnitude",
       legend = c("Low", "", "", "", "High"),
       fill = colorRampPalette(c("blue", "green", "yellow", "red"))(5),
       border = NA,
       cex = 0.7,
       bty = "n")

# Finish and save the PNG file
dev.off()
