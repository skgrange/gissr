context("Testing scrapers")

test_that("Test `sp_from_js_array`", {
  
  url <- "http://skgrange.github.io/www/maps/running_tracks/data/running_tracks_north_england_polylines.js"
  
  text <- readLines(url, warn = FALSE)
  
  # No object
  text <- stringr::str_split_fixed(text[1], "=", 2)[, 2]
  text <- stringr::str_replace(text, ",];", "]")
  
  # Scrape
  sp_points <- sp_from_js_array(text)
  sp_lines <- sp_from_js_array(text, type = "lines")
  sp_polygons <- sp_from_js_array(text, type = "polygons")
  
  # Test the types
  expect_equal(sp_class(sp_points), "SpatialPoints")
  expect_equal(sp_class(sp_lines), "SpatialLines")
  expect_equal(sp_class(sp_polygons), "SpatialPolygons")

})
