context("get.site.info")

test_that("get.site.info works with settings object", {
  # Create a simple settings object
  settings <- list(
    run = list(
      site = list(
        id = 1000000001,
        name = "Test Site",
        lat = 45.0,
        lon = -90.0
      )
    )
  )
  
  # Call get.site.info
  site_info <- get.site.info(settings)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 1)
  expect_equal(site_info$site_id, 1000000001)
  expect_equal(site_info$site_name, "Test Site")
  expect_equal(site_info$lat, 45.0)
  expect_equal(site_info$lon, -90.0)
  expect_equal(site_info$str_id, as.character(settings$run$site$id))
})

test_that("get.site.info works with CSV file", {
  # Create a temporary CSV file
  csv_file <- tempfile(fileext = ".csv")
  csv_data <- data.frame(
    site_id = c(1000000002, 1000000003),
    site_name = c("Site 1", "Site 2"),
    lat = c(40.0, 50.0),
    lon = c(-80.0, -100.0)
  )
  write.csv(csv_data, csv_file, row.names = FALSE)
  
  # Call get.site.info
  site_info <- get.site.info(csv_path = csv_file)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 2)
  expect_equal(site_info$site_id, c(1000000002, 1000000003))
  expect_equal(site_info$site_name, c("Site 1", "Site 2"))
  expect_equal(site_info$lat, c(40.0, 50.0))
  expect_equal(site_info$lon, c(-80.0, -100.0))
  expect_equal(site_info$str_id, as.character(csv_data$site_id))
  
  # Clean up
  unlink(csv_file)
})

test_that("get.site.info works with MultiSettings object", {
  # Create a MultiSettings object
  settings1 <- list(
    run = list(
      site = list(
        id = 1000000004,
        name = "Multi Site 1",
        lat = 35.0,
        lon = -85.0
      )
    )
  )
  
  settings2 <- list(
    run = list(
      site = list(
        id = 1000000005,
        name = "Multi Site 2",
        lat = 55.0,
        lon = -95.0
      )
    )
  )
  
  multi_settings <- structure(
    list(settings1, settings2),
    class = "MultiSettings"
  )
  
  # Call get.site.info
  site_info <- get.site.info(multi_settings)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 2)
  expect_equal(site_info$site_id, c(1000000004, 1000000005))
  expect_equal(site_info$site_name, c("Multi Site 1", "Multi Site 2"))
  expect_equal(site_info$lat, c(35.0, 55.0))
  expect_equal(site_info$lon, c(-85.0, -95.0))
  expect_equal(site_info$str_id, as.character(c(1000000004, 1000000005)))
})

test_that("get.site.info works with vectorized site information", {
  # Create a settings object with vectorized site information
  settings <- list(
    run = list(
      site = list(
        id = c(1000000006, 1000000007),
        name = c("Vector Site 1", "Vector Site 2"),
        lat = c(30.0, 60.0),
        lon = c(-75.0, -105.0)
      )
    )
  )
  
  # Call get.site.info
  site_info <- get.site.info(settings)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 2)
  expect_equal(site_info$site_id, c(1000000006, 1000000007))
  expect_equal(site_info$site_name, c("Vector Site 1", "Vector Site 2"))
  expect_equal(site_info$lat, c(30.0, 60.0))
  expect_equal(site_info$lon, c(-75.0, -105.0))
  expect_equal(site_info$str_id, as.character(c(1000000006, 1000000007)))
})

test_that("get.site.info validates coordinates with strict_checking", {
  # Create a settings object with invalid coordinates
  settings <- list(
    run = list(
      site = list(
        id = 1000000008,
        name = "Invalid Site",
        lat = 100.0,  # Invalid latitude
        lon = -180.0
      )
    )
  )
  
  # Call get.site.info with strict_checking = TRUE
  expect_error(get.site.info(settings, strict_checking = TRUE), 
               "Invalid latitude values")
  
  # Call get.site.info with strict_checking = FALSE
  site_info <- get.site.info(settings, strict_checking = FALSE)
  
  # Check the result
  expect_is(site_info, "data.frame")
  expect_equal(nrow(site_info), 1)
  expect_equal(site_info$site_id, 1000000008)
  expect_equal(site_info$site_name, "Invalid Site")
  expect_equal(site_info$lat, 100.0)
  expect_equal(site_info$lon, -180.0)
  expect_equal(site_info$str_id, as.character(settings$run$site$id))
})

test_that("str_id is correctly generated as a character string", {
  settings <- list(
    run = list(
      site = list(
        id = 1000000001,
        name = "Test Site",
        lat = 45.0,
        lon = -90.0
      )
    )
  )
  site_info <- get.site.info(settings)
  expect_type(site_info$str_id, "character")
  expect_equal(site_info$str_id, as.character(settings$run$site$id))

  # Test with CSV input
  csv_file <- tempfile(fileext = ".csv")
  csv_data <- data.frame(
    site_id = c(1000000002, 1000000003),
    site_name = c("Site 1", "Site 2"),
    lat = c(40.0, 50.0),
    lon = c(-80.0, -100.0)
  )
  write.csv(csv_data, csv_file, row.names = FALSE)
  site_info_csv <- get.site.info(csv_path = csv_file)
  expect_type(site_info_csv$str_id, "character")
  expect_equal(site_info_csv$str_id, as.character(csv_data$site_id))
  unlink(csv_file)
}) 