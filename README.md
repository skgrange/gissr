# **gissr**

**gissr** is a collection of R functions which make working with spatial data easier.

## To install:

The development version: `devtools::install_github("skgrange/gissr")`

## To-do: 

  1. Do some unit testing
  2. Work on documentation
  3. Get package on CRAN

## Background

R's spatial data analysis abilities are very well developed. Therefore, R can be used as an effective geographical information system (GIS). A key advantage of R in GIS applications is that the user can dip in-and-out of R's general string, numerical, and visualisation tools and apply them to spatial data.

However, the challenges I have with using R as a GIS include: keeping track of the multiple packages which are used, the lack of consistency among these packages, and the lack of tidy outputs which other areas of the R ecosystem have been so good at developing. To overcome this, I have written wrappers for many geographical functions which generally begin `sp_` to do particular tasks. Some of these functions will likely be useful for others. 

## Utility functions

  - Read shapefiles GPX, or GeoJSON files with `sp_read`.
  - Transform projection systems with `sp_transform`.
    - `sp_transform` can also force projections when a spatial object has none.
    - `transform_coordinates` does a similar thing, but for data frames.
  - Transform a data frames (tables) to a spatial objects with:
    - `data_frame_to_point`,
    - `data_frame_to_line`, and
    - `data_frame_to_polygon`.
  - Bind/combine spatial objects with `sp_bind` and `sp_bind_many`.
  - Calculate lengths or areas of spatial objects with `sp_area` and `sp_length`.
  - Transform a data frame with a well known text (WKT) variable to a spatial object with `sp_from_wkt`.
  - Clip or crop a spatial object to an rectangular envelope with `sp_crop`. 
    - To filter objects by other polygons, use `[` subsetting. 
  - "Dissolve" polygons to make a single feature with `sp_dissolve_polygons`.
  - Transform addresses/postcodes/other location strings to latitude and longitude pairs with `google_geocode`.
  - Export spatial objects and data frames to GPX or GeoJSON files with `write_gpx` and `write_geojson`. 
  - Fix issues with spatial objects with `sp_fix`. This function is a blatant wrap of [`cleangeo::clgeo_Clean`](https://github.com/eblondel/cleangeo). This function is a good piece of work so make sure you have a look at the **cleangeo** package.
  
## Fancy functions

  - Point-in-polygon tests with `left_join_spatial`.
  - Extract values from a raster layer with `left_join_raster`
  - Calculate distances between spatial objects with `sp_distance`.
    - `distance_by_haversine` does the same thing, but with a different method, and for data frames.
  - Simplify spatial objects with `sp_simplify`.
