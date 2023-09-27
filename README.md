# **gissr** <a href='https://github.com/skgrange/gissr'><img src='man/figures/logo.png' align="right" height="131.5" /></a>

[![Lifecycle Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/)

**gissr** is a collection of R functions which make working with spatial data easier.

## Installation

The development version: 

```
# Install dependency
remotes::install_github("skgrange/threadr")

# Install gissr
remotes::install_github("skgrange/gissr")
```

## Deprecation note

As most spatial data and R users will be aware, the **rgdal**, **rgeos**, and **maptools** packages will be returned in late 2023. This is because the developer of the packages is retiring and a new generation of spatial tools have emerged in the form of the **sf**, **stars**, and **terra** packages. Because **gissr** is mostly built upon the older packages, it will most likely be retired in 2023 too. The development of **gissr**'s successor based on the **sf** and  **terra** packages called [**sspatialr**](https://github.com/skgrange/sspatialr) is ongoing. For new projects, it is recommended that **sspatialr** is used rather than **gissr**.

## Background

R's spatial data analysis abilities are very well developed. Therefore, R can be used as an effective geographical information system (GIS). A key advantage of R in GIS applications is that the user can dip in-and-out of R's general string, numerical, and visualisation tools and apply them to spatial data.

However, the challenges I have with using R as a GIS are:

  - Keeping track of the multiple packages which are used,
  
  - the lack of consistency among these packages, and
  
  - the lack of tidy outputs which other areas of the R ecosystem have been so good at developing. 
  
To overcome these points, I have written wrappers for many geographical functions which generally begin `sp_` to do particular tasks and bundled all the dependencies together as a package. Some of these functions will likely be useful for others. 

## Utility functions

  - Easily read shapefiles, GPX, GeoJSON, KML, GML, TAB, and File Geodatabases with `sp_read`, a wrapper for `rgdal::readOGR`.
    - Also check spatial files and system things with `sp_list_drivers`, `sp_list_layers`, and `sp_layer_info`. 
    
  - Transform projection systems with `sp_transform`.
    - `sp_transform` can also force projections when a spatial object has none.
    - `transform_coordinates` does a similar thing, but for data frames.
    
  - Transform data frames (tables) to spatial points, lines, or polygons with `sp_from_data_frame`. 
  
  - Transform data frames with a well known text (WKT) variable (or just a vector) to a spatial object with `sp_from_wkt`.
  
  - Bind/combine different spatial objects with `sp_bind`. 
  
  - Unite spatial objects with `sp_unite` and do the opposite with `sp_disaggregate`. 
  
  - Calculate lengths or areas of spatial objects with `sp_area` and `sp_length`.
  
  - Clip or crop a spatial object to an rectangular envelope with `sp_clip`. 
  
    - To filter objects by other polygons, use `[` subsetting (or `sp_filter`). 
    
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
  
  - Create Tessellation polygons with `sp_tessellation_polygons`. 
  
  - Export spatial objects to spatial data files with `write_gpx`, `write_geojson`, and `write_shapefile`. 
  
  - Transform spatial objects to data frames with `sp_fortify`. 
  
### Raster functions

  - Create a raster layer from spatial data with `ra_from_sp`. 
  
  - Filter/crop/mask a raster layer with a spatial polygon with `ra_mask`. 
  
  - Interpolate a raster layer/surface with `ra_interpolate`. 
  
  - Increase a raster's resolution with `ra_disaggregate`. 
  
  - Smooth a raster's values with `ra_focal`.
  
  - Extract values from raster objects using spatial data types with `ra_drill` and then produce a "tidy data" version with `tidy_ra_drill`.
  
  - Transform raster objects to data frames with `ra_fortify`.
  
  - Bind or merge a number of raster layers together with `ra_bind` 
  
### OpenStreetMap data importers

  - A collection of `get_osm_*` functions to import data from OpenStreetMap. 

## Things I want to do

  - Develop a function which can read *n* features in a spatial data file. This will be helpful when large data files are encountered and system memory is too small to load the entire file at once. 
  
  - Get the interface between R and SpatiaLite sorted -- this can probably be left to [**sf**](https://github.com/r-spatial/sf) now.
  
  - Concave hull function *i.e.* find minimum area polygon.  
  
  - Add support for WKB (well-known binary). 

## See also

  - [**sf**](https://github.com/r-spatial/sf)
  