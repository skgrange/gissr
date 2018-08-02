# 
# Distance from coastline for UK places ---------------------------
library(gissr)
library(sp)
sp.places <- sp_read("/media/stuart/ELEMENTS/data_objects/osm_extracts/places")

# Give projection
sp.places <- sp_transform(sp.places)
# sp.places <- subset(sp.places, type %in% c("city", "town"))

sp.places <- sp.places[1:1000, ]

# Get a data frame
data.places <- data.frame(sp.places)

# Load coastlines
sp.coast.line <- sp_read("/media/stuart/ELEMENTS/data_objects/coastline/CoastLines")

# Give projection
sp.coast.line <- sp_transform(sp.coast.line)

# Load drawn line
data.drawn <- read.delim(
  "/media/stuart/ELEMENTS/data_objects/coastline/drawn_uk_coast_boundary.txt")

# Make a polygon
sp.drawn <- data_frame_to_polygon(data.drawn)

# Filter to drawn polygon
sp.coast.line <- sp.coast.line[sp.drawn, ]

# Find distances between sp objects
proj <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs "
data.places$distance <- sp_distance(sp.places, sp.coast.line, unit = "km", 
                                    cores = 4, proj = proj)

data.places <- data.places %>% 
  arrange(-distance)

library(microbenchmark)
benchmark <- microbenchmark(
  single = sp_distance(sp.places, sp.coast.line, unit = "km", cores = 1, proj = proj),
  double = sp_distance(sp.places, sp.coast.line, unit = "km", cores = 2, proj = proj),
  triple = sp_distance(sp.places, sp.coast.line, unit = "km", cores = 3, proj = proj),
  quad = sp_distance(sp.places, sp.coast.line, unit = "km", cores = 4, proj = proj),
  eight = sp_distance(sp.places, sp.coast.line, unit = "km", cores = 8, proj = proj),
  times = 10
)

ggplot2::autoplot(benchmark)

