where = "M62" # can be M62 or M18

# Check packages and install if needed
packages <- c("sf","tmap","dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Load Packages
lapply(packages, require, character.only = TRUE)
tmap_mode("view")

# Download Segments
download.file(paste0("https://raw.githubusercontent.com/mem48/OptiTruck/master/data/incidents/",where,"/clean/segments.geojson"), destfile = "segments.geojson")
segments = st_read("segments.geojson")

# Extract Lat/Lng Columns from the geometry column
coords = as.data.frame(st_coordinates(segments))

# bind onto orginal data
segments = bind_cols(segments, coords)

# Quick look at data format
head(segments)

# Plot Segments
qtm(segments, dots.col = "segment_id")


