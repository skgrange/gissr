context("Testing scrapers")

test_that("Test `scrape_gpx`", {
  
  # Get all gpx files included in the package
  file_list <- list.files(
    system.file("extdata", package = "gissr"), 
    pattern = "york.gpx$|lock_ness.gpx$",
    full.names = TRUE
  )
  
  # Use function
  df <- scrape_gpx(file_list, .id = "file")
  
  # Check return
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  
})
