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
  - Transform data frames (tables) to spatial points, lines, or polygons with `sp_from_data_frame`. 
  - Transform data frames with a well known text (WKT) variable (or just a vector) to a spatial object with `sp_from_wkt`.
  - Transform JavaScript coordinate arrays (for example: [[54.35221,-0.88518],[54.35237,-0.88544]]) into spatial objects with `sp_from_js_array`. 
  - Bind/combine different spatial objects with `sp_bind`. 
  - Unite spatial objects with `sp_unite` and do the opposite with `sp_disaggregate`. 
  - Calculate lengths or areas of spatial objects with `sp_area` and `sp_length`.
  - Clip or crop a spatial object to an rectangular envelope with `sp_clip`. 
    - To filter objects by other polygons, use `[` subsetting. 
    - Rectangular or elliptical polygons can be created with `sp_create_envelope` and `sp_ellipse` for this purpose too. 
  - Do simple transformations to spatial objects with `sp_move`, `sp_flip`, `sp_reflect`, and `sp_rotate`. 
  - Simplify spatial objects with `sp_simplify`.
  - "Dissolve" polygons to make a single feature with `sp_dissolve_polygons`.
  - "Punch" holes in polygons with `sp_punch`. 
  - Add positive or negative buffers with `sp_buffer`.
  - Create enclosing polygons with `sp_convex_hull`.
  - Find centroids of geometries with `sp_centroid`. 
  - "Promote" or "demote" Spatial\* to Spatial\*DataFrame, *i.e.* add or drop data slots for geometries with `sp_promote` and `sp_demote`. 
  - Return and reset geometry IDs with `sp_feature_ids` and `sp_reset_feature_ids`.
  - Point-in-polygon tests with `sp_left_join`.
  - Calculate distances among spatial objects with `sp_distance`.
    - `distance_by_haversine` does the same thing, but with a different method, and for data frames.
  - Fix issues with spatial objects with `sp_fix`. This function is a blatant wrap of [`cleangeo::clgeo_Clean`](https://github.com/eblondel/cleangeo). This function is a good piece of work so make sure you have a look at the **cleangeo** package.
  - Parse vectors of degrees, minutes, and seconds into decimal degrees with `dms_to_decimal`. 
  - Sort/arrange points in a clockwise order with `sort_points`. 
  - Export spatial objects to spatial data files with `write_gpx`, `write_geojson`, and `write_shapefile`. 

## Things I want to do

  - Develop a function which can read *n* features in a spatial data file. This will be helpful when large data files are encountered and system memory is too small to load the entire file at once. 
  - Get the interface between R and SpatiaLite sorted. 
  - Concave hull function *i.e.* find minimum area polygon.  
  - Add support for WKB. 
