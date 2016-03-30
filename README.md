# **gissr**

**gissr** is a collection of R functions which make working with spatial data easier.

## Installation

The development version: 
```
# Install dependency
devtools::install_github("skgrange/threadr")

# Install gissr
devtools::install_github("skgrange/gissr")
```

## To-do

  1. Do some more unit testing
  2. Work on documentation
  3. Get package on CRAN

## Background

R's spatial data analysis abilities are very well developed. Therefore, R can be used as an effective geographical information system (GIS). A key advantage of R in GIS applications is that the user can dip in-and-out of R's general string, numerical, and visualisation tools and apply them to spatial data.

However, the challenges I have with using R as a GIS include: keeping track of the multiple packages which are used, the lack of consistency among these packages, and the lack of tidy outputs which other areas of the R ecosystem have been so good at developing. To overcome this, I have written wrappers for many geographical functions which generally begin `sp_` to do particular tasks. Some of these functions will likely be useful for others. 

## Utility functions

  - Read: shapefiles, GPX, GeoJSON, KML, GML, TAB, and File Geodatabases with `sp_read`, a wrapper for `rgdal::readOGR`.
  - Transform projection systems with `sp_transform`.
    - `sp_transform` can also force projections when a spatial object has none.
    - `transform_coordinates` does a similar thing, but for data frames.
  - Transform data frames (tables) to spatial objects with:
    - `data_frame_to_points`,
    - `data_frame_to_lines`, and
    - `data_frame_to_polygons`.
  - Bind/combine spatial objects with `sp_bind` and `sp_bind_many`.
  - Calculate lengths or areas of spatial objects with `sp_area` and `sp_length`.
  - Transform a data frame with a well known text (WKT) variable (or just a vector) to a spatial object with `sp_from_wkt`.
  - Clip or crop a spatial object to an rectangular envelope with `sp_crop`. 
    - To filter objects by other polygons, use `[` subsetting. 
      - Rectangular or elliptical polygons can be created with `sp_create_envelope` and `sp_ellipse` for this purpose too. 
  - "Dissolve" polygons to make a single feature with `sp_dissolve_polygons`.
  - "Punch" holes in polygons with `sp_punch`. 
  - Add a positive or negative buffer with `sp_buffer`. 
  - "Promote" or "demote" Spatial\* to Spatial\*DataFrame, *i.e.* add or drop data slots for geometries. 
  - Export spatial objects to spatial files with `write_gpx`, `write_geojson`, and `write_shapefile`. 
  
## Fancy functions

  - Point-in-polygon tests with `left_join_spatial`.
  - Calculate distances between spatial objects with `sp_distance`.
    - `distance_by_haversine` does the same thing, but with a different method, and for data frames.
  - Simplify spatial objects with `sp_simplify`.
  - Fix issues with spatial objects with `sp_fix`. This function is a blatant wrap of [`cleangeo::clgeo_Clean`](https://github.com/eblondel/cleangeo). This function is a good piece of work so make sure you have a look at the **cleangeo** package.
