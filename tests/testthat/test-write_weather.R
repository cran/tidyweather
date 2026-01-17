test_that("write_weather basic functionality works", {
    # Read sample weather data
    file_path <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(file_path)
    
    # Create temporary file
    temp_file <- tempfile(fileext = ".met")
    
    # Write weather data
    result <- write_weather(records, temp_file, format = "APSIM")
    
    # Check that function returns file path invisibly
    expect_equal(result, temp_file)
    
    # Check that file was created
    expect_true(file.exists(temp_file))
    
    # Check file content exists and is not empty
    content <- readLines(temp_file)
    expect_gt(length(content), 0)
    new_records <- read_weather(temp_file)    
    expect_equal(new_records, records)
    
    # Clean up
    unlink(temp_file)
})

test_that("write_weather validates input parameters", {
    # Read sample data for testing
    file_path <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(file_path)
    temp_file <- tempfile(fileext = ".met")
    
    # Test invalid records input
    expect_error(
        write_weather("not_a_dataframe", temp_file),
        "is.data.frame\\(records\\) is not TRUE"
    )
    
    expect_error(
        write_weather(NULL, temp_file),
        "is.data.frame\\(records\\) is not TRUE"
    )
    
    # Test invalid file input
    expect_error(
        write_weather(records, NULL),
        "!is.null\\(file\\)"
    )
    
    expect_error(
        write_weather(records, "")
    )
    
    expect_error(
        write_weather(records, c("file1", "file2"))
    )
    
    # Test invalid format input
    expect_error(
        write_weather(records, temp_file, format = "INVALID")
    )
    
    expect_error(
        write_weather(records, temp_file, format = c("APSIM", "OTHER"))
    )
    
    # Test invalid overwrite input
    expect_error(
        write_weather(records, temp_file, overwrite = "yes"),
        "is.logical\\(overwrite\\)"
    )
    
    expect_error(
        write_weather(records, temp_file, overwrite = c(TRUE, FALSE)),
        "length\\(overwrite\\) == 1 is not TRUE"
    )
})

test_that("write_weather handles existing files correctly", {
    # Read sample data
    file_path <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(file_path)
    temp_file <- tempfile(fileext = ".met")
    
    # Write initial file
    write_weather(records, temp_file, format = "APSIM")
    expect_true(file.exists(temp_file))
    
    # Try to write again without overwrite (should fail)
    expect_error(
        write_weather(records, temp_file, format = "APSIM", overwrite = FALSE)
    )
    
    # Write again with overwrite (should succeed)
    expect_no_error(
        write_weather(records, temp_file, format = "APSIM", overwrite = TRUE)
    )
    
    # Clean up
    unlink(temp_file)
})

test_that("write_weather APSIM format produces correct structure", {
    # Read sample data
    file_path <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(file_path)
    temp_file <- tempfile(fileext = ".met")
    
    # Write weather data
    write_weather(records, temp_file, format = "APSIM")
    
    # Read back the content
    content <- readLines(temp_file)
    
    # Check for expected header components
    expect_true(any(grepl("!station number", content)))
    expect_true(any(grepl("!station name", content)))
    expect_true(any(grepl("latitude", content)))
    expect_true(any(grepl("longitude", content)))
    
    # Check for column headers
    expect_true(any(grepl("year", content)))
    expect_true(any(grepl("day", content)))
    expect_true(any(grepl("radn", content)))
    expect_true(any(grepl("maxt", content)))
    expect_true(any(grepl("mint", content)))
    expect_true(any(grepl("rain", content)))
    
    # Check for units line
    expect_true(any(grepl("\\(mj/m2\\)", content)))
    expect_true(any(grepl("\\(oC\\)", content)))
    expect_true(any(grepl("\\(mm\\)", content)))
    
    # Check that data rows exist
    data_rows <- content[!grepl("^!", content) & !grepl("^latitude", content) & 
                        !grepl("^longitude", content) & !grepl("^tav", content) & 
                        !grepl("^amp", content) & content != "" & 
                        !grepl("year", content) & !grepl("\\(", content)]
    expect_gt(length(data_rows), 0)
    
    # Clean up
    unlink(temp_file)
})

test_that("write_weather handles missing required columns", {
    # Create test data missing required columns
    incomplete_data <- tibble::tibble(
        year = 2023,
        day = 1:10,
        # Missing other required columns
        latitude = -35.0,
        longitude = 149.0,
        number = 72150,
        name = "Test Station"
    )
    
    temp_file <- tempfile(fileext = ".met")
    
    # Should fail when required columns are missing
    expect_error(
        write_weather(incomplete_data, temp_file),
        "Input data should have column"
    )
})

test_that("write_weather handles missing header information", {
    # Create data missing header info
    incomplete_header_data <- tibble::tibble(
        year = rep(2023, 10),
        day = 1:10,
        radn = rep(20, 10),
        maxt = rep(25, 10),
        mint = rep(15, 10),
        rain = rep(0, 10),
        evap = rep(5, 10),
        vp = rep(15, 10)
        # Missing latitude, longitude, number, name
    )
    
    temp_file <- tempfile(fileext = ".met")
    
    # Should fail when header columns are missing
    expect_error(
        write_weather(incomplete_header_data, temp_file),
        "Input data should have column"
    )
})

test_that("write_weather handles optional columns correctly", {
    # Create minimal data with only required columns
    minimal_data <- tibble::tibble(
        year = rep(2023, 5),
        day = 1:5,
        radn = rep(20, 5),
        maxt = rep(25, 5),
        mint = rep(15, 5),
        rain = rep(0, 5),
        evap = rep(5, 5),
        vp = rep(15, 5),
        latitude = -35.0,
        longitude = 149.0,
        number = 72150,
        name = "Test Station"
    )
    
    temp_file <- tempfile(fileext = ".met")
    
    # Should work with minimal required data
    expect_no_error(
        write_weather(minimal_data, temp_file)
    )
    
    # Check that file was created
    expect_true(file.exists(temp_file))
    
    # Check content
    content <- readLines(temp_file)
    expect_gt(length(content), 0)
    
    # Should NOT contain optional columns that weren't provided
    expect_false(any(grepl("tav", content)))
    expect_false(any(grepl("amp", content)))
    expect_false(any(grepl("rhmint", content)))
    expect_false(any(grepl("rhmaxt", content)))
    expect_false(any(grepl("code", content)))
    
    # Clean up
    unlink(temp_file)
})

test_that("write_weather handles multiple values in header fields", {
    # Create data with multiple values in header fields (should fail)
    invalid_header_data <- tibble::tibble(
        year = rep(2023, 5),
        day = 1:5,
        radn = rep(20, 5),
        maxt = rep(25, 5),
        mint = rep(15, 5),
        rain = rep(0, 5),
        evap = rep(5, 5),
        vp = rep(15, 5),
        latitude = c(-35.0, -35.1, -35.0, -35.0, -35.0), # Multiple values
        longitude = 149.0,
        number = 72150,
        name = "Test Station"
    )
    
    temp_file <- tempfile(fileext = ".met")
    
    # Should fail when header fields have multiple values
    expect_error(
        write_weather(invalid_header_data, temp_file),
        "Input data should have only one unique value for latitude"
    )
})

test_that("write_weather format parameter is case insensitive", {
    # Read sample data
    file_path <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(file_path)
    temp_file1 <- tempfile(fileext = ".met")
    temp_file2 <- tempfile(fileext = ".met")
    temp_file3 <- tempfile(fileext = ".met")
    
    # Test different case variations
    expect_no_error(write_weather(records, temp_file1, format = "APSIM"))
    expect_no_error(write_weather(records, temp_file2, format = "apsim"))
    expect_no_error(write_weather(records, temp_file3, format = "Apsim"))
    
    # All should create files
    expect_true(file.exists(temp_file1))
    expect_true(file.exists(temp_file2))
    expect_true(file.exists(temp_file3))
    
    # Clean up
    unlink(c(temp_file1, temp_file2, temp_file3))
})

test_that("write_weather return value", {
    # Read sample data
    file_path <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(file_path)
    temp_file <- tempfile(fileext = ".met")
    
    # Function should return file path invisibly
    result <- write_weather(records, temp_file)
    expect_equal(result, temp_file)
    expect_invisible(write_weather(records, temp_file, overwrite = TRUE))
    
    # Clean up
    unlink(temp_file)
})

test_that("write_weather works with complete dataset including optional columns", {
    # Create complete dataset
    complete_data <- tibble::tibble(
        year = rep(2023, 3),
        day = 1:3,
        radn = c(20.5, 21.0, 19.5),
        maxt = c(25.2, 26.1, 24.8),
        mint = c(15.1, 14.9, 15.5),
        rain = c(0, 2.5, 0),
        evap = c(5.2, 4.8, 5.5),
        vp = c(15.1, 15.3, 14.9),
        rhmint = c(65, 70, 60),
        rhmaxt = c(85, 90, 80),
        code = c(1, 1, 1),
        latitude = -35.2809,
        longitude = 149.1300,
        number = 72150,
        name = "Canberra Airport",
        tav = 13.2,
        amp = 12.4
    )
    
    temp_file <- tempfile(fileext = ".met")
    
    # Should work with complete data
    expect_no_error(write_weather(complete_data, temp_file))
    
    # Check that file was created
    expect_true(file.exists(temp_file))
    
    # Check content includes optional fields
    content <- readLines(temp_file)
    expect_true(any(grepl("tav", content)))
    expect_true(any(grepl("amp", content)))
    expect_true(any(grepl("rhmint", content)))
    expect_true(any(grepl("rhmaxt", content)))
    
    # Clean up
    unlink(temp_file)
})

test_that("write_weather documentation examples work", {
    # Test the examples from the documentation
    
    # Read sample weather data from package
    file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(file)
    
    # Write to temporary file
    temp_file <- tempfile(fileext = ".met")
    
    # Write to APSIM format
    expect_no_error(write_weather(records, temp_file, format = "APSIM"))
    expect_true(file.exists(temp_file))
    
    # Overwrite existing file
    expect_no_error(write_weather(records, temp_file, format = "APSIM", overwrite = TRUE))
    
    # Clean up
    unlink(temp_file)
})

# Clean up function for all tests
test_that("cleanup after write_weather tests", {
    # Clean up any remaining temporary files
    temp_files <- list.files(tempdir(), pattern = "^test.*\\.met$", full.names = TRUE)
    if (length(temp_files) > 0) {
        unlink(temp_files)
    }
    expect_true(TRUE) # Just to have an assertion
})
