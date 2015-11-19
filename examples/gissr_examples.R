# Set-up ---------------------------
# Load packages
library(threadr)
library(dplyr)
library(tidyr)
library(gissr)
library(mover)

# Set options
options(stringsAsFactors = FALSE)

# Set working directory
setwd("~/Dropbox/R/package_development/gissr/examples")

# Load e-reporting xml document
data_station <- flatten_xml("http://cdr.eionet.europa.eu/fr/eu/aqd/d/envvjihta/FR_DATASET-D-2014_V2_20151029.xml", 
                            "station")

# Select a few of variables
data_station_select <- data_station %>% 
  select(gml_id, ef_name, aqd_eu_station_code, ef_geometry_gml_point_gml_pos_text,
         aqd_municipality, aqd_altitude_text) %>% 
  tidyr::separate(ef_geometry_gml_point_gml_pos_text, c("latitude", "longitude"), " ",
                  convert = TRUE)

# Load raster object
raster_ozone <- readRDS("../data/ozone_forecast_raster.rds")

# Join values
data_station_select_join <- left_join_raster(data_station_select, 
                                             raster = raster_ozone)




# Or some other points
left_join_raster(data.frame(latitude = c(51.454054, 51.513674), 
                            longitude = c(-0.990327, -0.099885)), 
                 raster = raster)

