# **gissr**

**gissr** is a collection of R functions which make working with spatial data easier.

## To install:

The development version: `devtools::install_github("skgrange/gissr")`

## To-do: 

  1. Do some unit testing
  2. Work on documentation
  3. Get package on CRAN

## Background

R's spatial data analysis abilities are very well developed. Therefore, R can be used as a rather effective geographical information system (GIS). A key advantage of R in GIS applications is that the user can dip in-and-out of R's general string, numerical, and visualisation tools and apply them to spatial data.

However, the challenges I have had with using R as a GIS have included: keeping track of the multiple packages which are used, the lack of consistency, and the lack of tidy outputs which other areas of the R ecosystem have been so good at developing. To overcome this, I have written wrappers for many geographical functions which generally begin `sp_` to do particular tasks. Some of these functions will likely be useful for others too. 

## Utility functions

  - Reading shapefiles or GPX files with `sp_read`.
  - Transforming projection systems with `sp_transform`.
    - `sp_transform` can also force projections when a spatial object has none.
    - `transform_coordinates` does a similar thing, but for data frames.
  - Transforming a data frame to a spatial line or spatial polygon object with `data_frame_to_line` and `data_frame_to_polygon`.
  - Binding/combining spatial objects with `sp_bind` or `sp_bind_many`.
  - Calculate lengths or areas of spatial objects with `sp_area` and `sp_length`.
  - Transform addresses/postcodes/other location strings to latitude and longitude pairs with `google_geocode`.
  - Export spatial objects and data frames to GPX files with `write.gpx`. 
  
## Fancy functions

  - Point-in-polygon tests with `left_join_spatial`.
  - Calculate distances between spatial objects with `sp_distance`.
    - `distance_by_haversine` does the same thing, but with a different method, and for data frames.
  - Simplify spatial objects with `sp_simplify`.
