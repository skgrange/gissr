context("Reading spatial data")

test_that("Test `sp_read` for gpx files", {
  
  sp_gpx_tracks <- sp_read("../../data/york.gpx", verbose = FALSE)
  sp_gpx_routes <- sp_read("../../data/northcote-tavern-5-km-run.gpx", 
                           layer = "routes", verbose = FALSE)
  
  # Create points
  # a <- as(sp_gpx_routes, "SpatialPointsDataFrame")
  # a@data <- threadr::drop_na_columns(a@data)
  # write_gpx(a, "../../data/northcote-tavern-5-km-run_points.gpx")
  
  sp_gpx_points <- sp_read("../../data/northcote-tavern-5-km-run_points.gpx", 
                           layer = "points", verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_gpx_tracks)[1], "SpatialLinesDataFrame")
  expect_equal(class(sp_gpx_routes)[1], "SpatialLinesDataFrame")
  expect_equal(class(sp_gpx_points)[1], "SpatialPointsDataFrame")
  
})


test_that("Test `sp_read` for shapefiles", {
  
  sp_shape <- sp_read("../../data/shapefile/christchurch-city-building-footprints", 
                      verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_shape)[1], "SpatialPolygonsDataFrame")

})


test_that("Test `sp_read` for mapinfo files", {
  
  sp_tab <- sp_read("../../data/mapinfo/christchurch-city-building-footprints", 
                    verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_tab)[1], "SpatialPolygonsDataFrame")
  
})


test_that("Test `sp_read` for json files", {
  
  sp_json <- sp_read("../../data/luxembourg_monitoring_stations.json", 
                     verbose = FALSE)
  sp_json_points <- sp_read("../../data/points_and_lines.json", geom = "points", 
                            verbose = FALSE)
  sp_json_lines <- sp_read("../../data/points_and_lines.json", geom = "lines", 
                           verbose = FALSE)
  sp_json_polygons <- sp_read("../../data/points_and_lines.json", geom = "polygons", 
                              verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_json)[1], "SpatialPointsDataFrame")
  expect_equal(class(sp_json_points)[1], "SpatialPointsDataFrame")
  expect_equal(class(sp_json_lines)[1], "SpatialLinesDataFrame")
  expect_equal(class(sp_json_polygons)[1], "SpatialPolygonsDataFrame")
  
})


test_that("Test `sp_read` for kml files", {
  
  # 
  sp_kml <- sp_read("../../data/time-stamp-point.kml", verbose = FALSE)
  
  # No geom over ride used
  expect_error(sp_read("../../data/york.kml", verbose = FALSE))
  
  sp_kml_lines <- sp_read("../../data/york.kml", geom = "lines", verbose = FALSE)
  sp_kml_points <- sp_read("../../data/york.kml", geom = "points", verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_kml_lines)[1], "SpatialLinesDataFrame")
  expect_equal(class(sp_kml_points)[1], "SpatialPointsDataFrame")

})


test_that("Test `sp_read` for gml files", {
  
  # GML to GeoJSON conversion
  suppressWarnings(
    sp_gml <- sp_read("../../data/2013_G_GB_Attainment.xml", 
                      layer = "AQD_Attainment", verbose = FALSE)
  )

  # Test the types
  expect_equal(class(sp_gml)[1], "SpatialPolygonsDataFrame")
  
})


test_that("Test `sp_read` for Geodatabase", {
  
  # GML to GeoJSON conversion
  sp <- sp_read("../../data/World.gdb", "Yemen", verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp)[1], "SpatialPolygonsDataFrame")
  
})