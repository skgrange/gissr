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

  1. Travis builds are breaking because of the package's dependence on system-spatial packages; needs to be resolved. 
  2. Do some more unit testing
  3. Get package on CRAN

## Background

R's spatial data analysis abilities are very well developed. Therefore, R can be used as an effective geographical information system (GIS). A key advantage of R in GIS applications is that the user can dip in-and-out of R's general string, numerical, and visualisation tools and apply them to spatial data.

However, the challenges I have with using R as a GIS are:

  - Keeping track of the multiple packages which are used,
  - the lack of consistency among these packages, and
  - the lack of tidy outputs which other areas of the R ecosystem have been so good at developing. 
  
To overcome these points, I have written wrappers for many geographical functions which generally begin `sp_` to do particular tasks and bundled all the dependencies together as a package. Some of these functions will likely be useful for others. 

## Utility functions

  - Read shapefiles, GPX, GeoJSON, KML, GML, TAB, and File Geodatabases with `sp_read`, a wrapper for `rgdal::readOGR`.
    - Also check spatial file and system things with `sp_list_drivers`, `sp_list_layers`, and `sp_layer_info`. 
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
  - Create enclosing polygons with `sp_convex_hull`. 
  - Find centroids of geometries with `sp_centroid`. 
  - "Promote" or "demote" Spatial\* to Spatial\*DataFrame, *i.e.* add or drop data slots for geometries with `sp_promote` and `sp_demote`. 
  - Return and reset geometry IDs with `sp_feature_ids` and `sp_reset_feature_ids`.
  - Export spatial objects to spatial files with `write_gpx`, `write_geojson`, and `write_shapefile`. 
  
## Fancy functions

  - Point-in-polygon tests with `left_join_spatial`.
  - Calculate distances among spatial objects with `sp_distance`.
    - `distance_by_haversine` does the same thing, but with a different method, and for data frames.
  - Simplify spatial objects with `sp_simplify`.
  - Fix issues with spatial objects with `sp_fix`. This function is a blatant wrap of [`cleangeo::clgeo_Clean`](https://github.com/eblondel/cleangeo). This function is a good piece of work so make sure you have a look at the **cleangeo** package.
