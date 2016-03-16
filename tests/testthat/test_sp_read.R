context("Reading spatial data")

test_that("Test `sp_read` for gpx files", {
  
  # Where the files are located
  file_path <- system.file("extdata", package = "gissr")
  
  sp_gpx_tracks <- sp_read(file.path(file_path, "york.gpx"), verbose = FALSE)
  sp_gpx_routes <- sp_read(file.path(file_path, "northcote-tavern-5-km-run.gpx"),
                           layer = "routes", verbose = FALSE)
  
  # Create points
  # a <- as(sp_gpx_routes, "SpatialPointsDataFrame")
  # a@data <- threadr::drop_na_columns(a@data)
  # write_gpx(a, "../../data/northcote-tavern-5-km-run_points.gpx")
  
  sp_gpx_points <- sp_read(file.path(file_path, "northcote-tavern-5-km-run_points.gpx"),
                           layer = "points", verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_gpx_tracks)[1], "SpatialLinesDataFrame")
  expect_equal(class(sp_gpx_routes)[1], "SpatialLinesDataFrame")
  expect_equal(class(sp_gpx_points)[1], "SpatialPointsDataFrame")
  
})


test_that("Test `sp_read` for shapefiles", {
  
  # Where the files are located
  file_path <- system.file("extdata/shapefile", package = "gissr")
  
  sp_shape <- sp_read(file.path(file_path, "christchurch-city-building-footprints"), 
                      verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_shape)[1], "SpatialPolygonsDataFrame")

})


test_that("Test `sp_read` for mapinfo files", {
  
  # Where the files are located
  file_path <- system.file("extdata/mapinfo", package = "gissr")
  
  sp_tab <- sp_read(file.path(file_path, "christchurch-city-building-footprints"), 
                    verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_tab)[1], "SpatialPolygonsDataFrame")
  
})


test_that("Test `sp_read` for json files", {
  
  # Where the files are located
  file_path <- system.file("extdata", package = "gissr")
  
  sp_json <- sp_read(file.path(file_path, "luxembourg_monitoring_stations.json"), 
                     verbose = FALSE)
  
  sp_json_points <- sp_read(file.path(file_path, "points_and_lines.json"), 
                            geom = "points", verbose = FALSE)
  
  sp_json_lines <- sp_read(file.path(file_path, "points_and_lines.json"), 
                           geom = "lines", verbose = FALSE)
  
  # Not a useful file name...
  sp_json_polygons <- sp_read(file.path(file_path, "points_and_lines.json"), 
                              geom = "polygons", verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp_json)[1], "SpatialPointsDataFrame")
  expect_equal(class(sp_json_points)[1], "SpatialPointsDataFrame")
  expect_equal(class(sp_json_lines)[1], "SpatialLinesDataFrame")
  expect_equal(class(sp_json_polygons)[1], "SpatialPolygonsDataFrame")
  
})


test_that("Test `sp_read` for kml files", {
  
  # I do not think Windows drivers are avaliable for kml
  if (.Platform$OS.type == "unix") {
    
    # Where the files are located
    file_path <- system.file("extdata", package = "gissr")
    
    sp_kml <- sp_read(file.path(file_path, "time-stamp-point.kml"), verbose = FALSE)
    
    # No geom override used
    expect_error(sp_read(file.path(file_path, "york.kml"), verbose = FALSE))
    
    sp_kml_lines <- sp_read(file.path(file_path, "york.kml"), geom = "lines", 
                            verbose = FALSE)
    
    sp_kml_points <- sp_read(file.path(file_path, "york.kml"), geom = "points", 
                             verbose = FALSE)
    
    # Test the types
    expect_equal(class(sp_kml_lines)[1], "SpatialLinesDataFrame")
    expect_equal(class(sp_kml_points)[1], "SpatialPointsDataFrame")
    
  }
  
})


test_that("Test `sp_read` for gml files", {
  
  # Where the files are located
  file_path <- system.file("extdata", package = "gissr")
  
  # GML
  suppressWarnings(
    sp_gml <- sp_read(file.path(file_path, "2013_G_GB_Attainment.xml"), 
                      layer = "AQD_Attainment", verbose = FALSE)
  )

  # Test the types
  expect_equal(class(sp_gml)[1], "SpatialPolygonsDataFrame")
  
})


test_that("Test `sp_read` for Geodatabase", {
  
  # Where the files are located
  file_path <- system.file("extdata", package = "gissr")
  
  
  # Geodatabase
  sp <- sp_read(file.path(file_path, "World.gdb"), "Yemen", verbose = FALSE)
  
  # Test the types
  expect_equal(class(sp)[1], "SpatialPolygonsDataFrame")
  
})
