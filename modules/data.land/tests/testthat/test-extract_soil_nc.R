context("extract_soil_gssurgo")

test_that("extract_soil_gssurgo returns valid NetCDF files for valid coordinates", {
  skip_on_cran()
  skip_on_ci()
  lat <- 40.0
  lon <- -88.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir, 
    lat = lat, 
    lon = lon,
    size = 2,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15, 0.30)
  )
  
  # Test return value structure
  expect_true(is.list(res) || is.vector(res))
  expect_gt(length(res), 0)
  expect_true(all(names(res) == "path"))
  
  # Validate all files exist and contain valid data
  file_paths <- unlist(res)
  expect_true(all(file.exists(file_paths)))
  
  # Validate NetCDF content
  if (requireNamespace("ncdf4", quietly = TRUE)) {
    expected_vars <- c("fraction_of_sand_in_soil", "fraction_of_silt_in_soil", 
                      "fraction_of_clay_in_soil", "soil_depth", "soil_organic_carbon_stock")
    for (ncfile in file_paths) {
      nc <- ncdf4::nc_open(ncfile)
      # Check required variables exist
      for (var in expected_vars) {
        expect_true(var %in% names(nc$var), 
                   info = paste("Missing variable", var, "in", ncfile))
      }
      sand <- ncdf4::ncvar_get(nc, "fraction_of_sand_in_soil")
      silt <- ncdf4::ncvar_get(nc, "fraction_of_silt_in_soil")
      clay <- ncdf4::ncvar_get(nc, "fraction_of_clay_in_soil")
      soc <- ncdf4::ncvar_get(nc, "soil_organic_carbon_stock")
    
      expect_true(length(sand) > 0 && all(!is.na(sand)), 
                 info = "Sand data contains NA or is empty")
      expect_true(length(soc) > 0 && all(!is.na(soc)), 
                 info = "SOC data contains NA or is empty")
      
      # Check soil texture sum constraint (sand + silt + clay ~ 1)
      if (length(sand) == length(silt) && length(silt) == length(clay)) {
        texture_sum <- sand + silt + clay
        expect_true(all(abs(texture_sum - 1) < 0.01), 
                   info = "Soil texture fractions do not sum to 1")
      }
      # Check realistic value ranges
      expect_true(all(sand >= 0 & sand <= 1), info = "Sand fraction out of range [0,1]")
      expect_true(all(silt >= 0 & silt <= 1), info = "Silt fraction out of range [0,1]")
      expect_true(all(clay >= 0 & clay <= 1), info = "Clay fraction out of range [0,1]")
      expect_true(all(soc >= 0), info = "SOC values should be non-negative")
      ncdf4::nc_close(nc)
    }
  }
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo handles invalid coordinates gracefully", {
  skip_on_cran()
  skip_on_ci()
  lat <- 0.0
  lon <- 0.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)

  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 1,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15)
  )
  expect_null(res)
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo validates input parameters", {
  skip_on_cran()
  skip_on_ci()
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  # Test missing latitude
  expect_error(
    extract_soil_gssurgo(
      outdir = tmp_outdir, 
      lat = NULL, 
      lon = -88.0,
      size = 1, 
      grid_size = 3, 
      grid_spacing = 10
    ),
    regexp = "lat"
  )
  
  # Test missing longitude
  expect_error(
    extract_soil_gssurgo(
      outdir = tmp_outdir, 
      lat = 40.0, 
      lon = NULL,
      size = 1, 
      grid_size = 3, 
      grid_spacing = 10
    ),
    regexp = "lon"
  )
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo creates output directory when missing", {
  skip_on_cran()
  skip_on_ci()
  lat <- 40.0
  lon <- -88.0
  tmp_outdir <- tempfile("gssurgo_test_")
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 1,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15)
  )
  
  expect_true(dir.exists(tmp_outdir))
  if (!is.null(res)) {
    expect_true(all(file.exists(unlist(res))))
  }
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo scales appropriately with ensemble size", {
  skip_on_cran()
  skip_on_ci()
  lat <- 40.0
  lon <- -88.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 5,
    grid_size = 5,
    grid_spacing = 100,
    depths = c(0.15, 0.30, 0.60)
  )
  
  expect_true(is.list(res) || is.vector(res))
  if (!is.null(res)) {
    expect_gt(length(res), 1)
    file_paths <- unlist(res)
    expect_true(all(file.exists(file_paths)))
    # Basic validation of generated files
    if (requireNamespace("ncdf4", quietly = TRUE)) {
      for (ncfile in file_paths) {
        nc <- ncdf4::nc_open(ncfile)
        expect_true("fraction_of_sand_in_soil" %in% names(nc$var))
        expect_true("soil_organic_carbon_stock" %in% names(nc$var))
        sand <- ncdf4::ncvar_get(nc, "fraction_of_sand_in_soil")
        soc <- ncdf4::ncvar_get(nc, "soil_organic_carbon_stock")
        expect_true(length(sand) > 0 && all(!is.na(sand)))
        expect_true(length(soc) > 0 && all(!is.na(soc)))
        ncdf4::nc_close(nc)
      }
    }
  }
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo handles different depth configurations", {
  skip_on_cran()
  skip_on_ci()
  lat <- 40.0
  lon <- -88.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  
  # Test with single depth
  res_single <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 1,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15)
  )
  
  # Test with multiple depths
  subdir <- file.path(tmp_outdir, "multiple")
  dir.create(subdir, recursive = TRUE)
  res_multiple <- extract_soil_gssurgo(
    outdir = subdir,
    lat = lat,
    lon = lon,
    size = 1,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15, 0.30, 0.60, 1.0)
  )
  if (!is.null(res_single)) {
    expect_true(all(file.exists(unlist(res_single))))
  }
  if (!is.null(res_multiple)) {
    expect_true(all(file.exists(unlist(res_multiple))))
  }
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo handles edge cases in soil data processing", {
  skip_on_cran()
  skip_on_ci()
  lat <- 40.0
  lon <- -88.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  
  # Test with minimal grid (single point)
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 1,
    grid_size = 1,
    grid_spacing = 1,
    depths = c(0.15)
  )
  
  if (!is.null(res)) {
    expect_true(is.list(res) || is.vector(res))
    expect_true(all(file.exists(unlist(res))))
  }
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo maintains data consistency across ensemble members", {
  skip_on_cran()
  skip_on_ci()
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    skip("ncdf4 package not available")
  }
  lat <- 40.0
  lon <- -88.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 3,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15, 0.30)
  )
  
  if (!is.null(res) && length(res) > 1) {
    file_paths <- unlist(res)
    # Check that all files have same structure
    first_nc <- ncdf4::nc_open(file_paths[1])
    first_vars <- names(first_nc$var)
    first_dims <- sapply(first_nc$var, function(v) v$size)
    ncdf4::nc_close(first_nc)
    for (i in 2:length(file_paths)) {
      nc <- ncdf4::nc_open(file_paths[i])
      vars <- names(nc$var)
      dims <- sapply(nc$var, function(v) v$size)
      ncdf4::nc_close(nc)
      expect_equal(vars, first_vars, 
                  info = paste("Variable names differ between ensemble members"))
      expect_equal(dims, first_dims, 
                  info = paste("Dimension sizes differ between ensemble members"))
    }
  }
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo returns NULL when no valid soil data is found", {
  skip_on_cran()
  skip_on_ci()
  lat <- 0.0
  lon <- 0.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 1,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15)
  )
  expect_null(res) # Error: No mapunit keys were found for this site.
  unlink(tmp_outdir, recursive = TRUE)
})

test_that("extract_soil_gssurgo performance is reasonable for typical use cases", {
  skip_on_cran()
  skip_on_ci()
  lat <- 40.0
  lon <- -88.0
  tmp_outdir <- tempfile("gssurgo_test_")
  dir.create(tmp_outdir, recursive = TRUE)
  
  # Test execution time for reasonable parameters
  start_time <- Sys.time()
  res <- extract_soil_gssurgo(
    outdir = tmp_outdir,
    lat = lat,
    lon = lon,
    size = 2,
    grid_size = 3,
    grid_spacing = 10,
    depths = c(0.15, 0.30)
  )
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  # Should complete within reasonable time(5 minutes threshold)
  expect_lt(execution_time, 300, 
           info = "Function execution time exceeds 5 minutes")
  
  if (!is.null(res)) {
    expect_true(all(file.exists(unlist(res))))
  }
  unlink(tmp_outdir, recursive = TRUE)
})