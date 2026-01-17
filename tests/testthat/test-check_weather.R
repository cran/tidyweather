test_that("check_weather returns valid result for good data", {
    # Create a sample dataset with continuous dates and valid values
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day"),
        mint = c(10, 12, 11, 9, 13, 14, 12, 10, 11, 13),
        maxt = c(25, 27, 26, 24, 28, 29, 27, 25, 26, 28),
        radn = c(20, 22, 21, 19, 23, 24, 22, 20, 21, 23),
        rain = c(0, 5, 0, 10, 0, 0, 2, 0, 8, 0),
        latitude = rep(-35.0, 10),
        longitude = rep(147.0, 10)
    )
    
    result <- check_weather(data)
    
    expect_true(result$is_valid)
    expect_null(result$date_gaps)
    expect_null(result$missing_values)
    expect_null(result$extreme_values)
})


test_that("check_weather detects date gaps", {
    # Create data with a date gap
    data <- data.frame(
        date = c(as.Date("2020-01-01"), as.Date("2020-01-02"), 
                as.Date("2020-01-05"), as.Date("2020-01-06")),
        mint = c(10, 12, 11, 9),
        maxt = c(25, 27, 26, 24),
        radn = c(20, 22, 21, 19),
        rain = c(0, 5, 0, 10),
        latitude = rep(-35.0, 4),
        longitude = rep(147.0, 4)
    )
    
    result <- check_weather(data)
    
    expect_false(result$is_valid)
    expect_false(is.null(result$date_gaps))
    expect_equal(nrow(result$date_gaps), 1)
    expect_equal(result$date_gaps$days_missing, 2)
})


test_that("check_weather detects missing values", {
    # Create data with missing values
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, NA, 11, 9, 13),
        maxt = c(25, 27, 26, NA, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, NA),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    result <- check_weather(data)
    
    expect_false(result$is_valid)
    expect_false(is.null(result$missing_values))
    expect_equal(nrow(result$missing_values), 3)
    expect_true("mint" %in% result$missing_values$column)
    expect_true("maxt" %in% result$missing_values$column)
    expect_true("rain" %in% result$missing_values$column)
})


test_that("check_weather detects extreme temperature values", {
    # Create data with extreme temperature values
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(-150, 12, 11, 9, 13),  # -150 is extreme
        maxt = c(25, 27, 150, 24, 28),  # 150 is extreme
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    result <- check_weather(data)
    
    expect_false(result$is_valid)
    expect_false(is.null(result$extreme_values))
    expect_true(nrow(result$extreme_values) >= 2)
})


test_that("check_weather detects extreme radiation values", {
    # Create data with extreme radiation values
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(-5, 22, 21, 60, 23),  # -5 and 60 are extreme
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    result <- check_weather(data)
    
    expect_false(result$is_valid)
    expect_false(is.null(result$extreme_values))
    expect_true(nrow(result$extreme_values) >= 2)
})


test_that("check_weather detects extreme rainfall values", {
    # Create data with extreme rainfall values
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(-10, 5, 0, 600, 0),  # -10 and 600 are extreme
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    result <- check_weather(data)
    
    expect_false(result$is_valid)
    expect_false(is.null(result$extreme_values))
    expect_true(nrow(result$extreme_values) >= 2)
})


test_that("check_weather stops on error when requested", {
    # Create data with issues
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, NA, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    expect_error(check_weather(data, stop_on_error = TRUE))
})


test_that("check_weather requires date column", {
    # Create data without date column
    data <- data.frame(
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    expect_error(check_weather(data), "must contain a 'date' column")
})


test_that("check_weather requires key columns", {
    # Create data missing a key column
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
        # rain is missing
    )
    
    expect_error(check_weather(data), "Missing required columns")
})


test_that("check_weather works with custom ranges", {
    # Create data that would be valid with custom ranges
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    # Test with very restrictive temperature range
    result <- check_weather(data, temp_range = c(0, 15))
    expect_false(result$is_valid)
    expect_false(is.null(result$extreme_values))
    
    # Test with very permissive temperature range
    result <- check_weather(data, temp_range = c(-200, 200))
    expect_true(result$is_valid)
})


test_that("check_weather handles single day data", {
    # Create single day dataset
    data <- data.frame(
        date = as.Date("2020-01-01"),
        mint = 10,
        maxt = 25,
        radn = 20,
        rain = 0,
        latitude = -35.0,
        longitude = 147.0
    )
    
    result <- check_weather(data)
    
    expect_true(result$is_valid)
    expect_null(result$date_gaps)
})


test_that("check_weather detects multiple issues simultaneously", {
    # Create data with multiple types of issues
    data <- data.frame(
        date = c(as.Date("2020-01-01"), as.Date("2020-01-02"), 
                as.Date("2020-01-05")),  # Date gap
        mint = c(10, NA, -150),  # Missing value and extreme value
        maxt = c(25, 27, 26),
        radn = c(20, 22, 21),
        rain = c(0, 5, 0),
        latitude = rep(-35.0, 3),
        longitude = rep(147.0, 3)
    )
    
    result <- check_weather(data)
    
    expect_false(result$is_valid)
    expect_false(is.null(result$date_gaps))
    expect_false(is.null(result$missing_values))
    expect_false(is.null(result$extreme_values))
})


test_that("check_weather accepts single latitude value", {
    # Create data with consistent latitude
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    result <- check_weather(data)
    expect_true(result$is_valid)
})


test_that("check_weather rejects multiple latitude values", {
    # Create data with inconsistent latitude
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = c(-35.0, -35.1, -35.2, -35.0, -35.1),
        longitude = rep(147.0, 5)
    )
    
    expect_error(
        check_weather(data),
        "Latitude column should contain a single value for all records"
    )
})


test_that("check_weather accepts single longitude value", {
    # Create data with consistent longitude
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(147.0, 5)
    )
    
    result <- check_weather(data)
    expect_true(result$is_valid)
})


test_that("check_weather rejects multiple longitude values", {
    # Create data with inconsistent longitude
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = c(147.0, 147.1, 147.2, 147.0, 147.1)
    )
    
    expect_error(
        check_weather(data),
        "Longitude column should contain a single value for all records"
    )
})


test_that("check_weather requires latitude column", {
    # Create data without latitude column
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        longitude = rep(147.0, 5)
    )
    
    expect_error(check_weather(data), "must contain a 'latitude' column")
})


test_that("check_weather requires longitude column", {
    # Create data without longitude column
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5)
    )
    
    expect_error(check_weather(data), "must contain a 'longitude' column")
})


test_that("check_weather rejects NA latitude values", {
    # Create data with NA latitude
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(NA_real_, 5),
        longitude = rep(147.0, 5)
    )
    
    expect_error(check_weather(data), "Latitude column contains only NA values")
})


test_that("check_weather rejects NA longitude values", {
    # Create data with NA longitude
    data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
        mint = c(10, 12, 11, 9, 13),
        maxt = c(25, 27, 26, 24, 28),
        radn = c(20, 22, 21, 19, 23),
        rain = c(0, 5, 0, 10, 0),
        latitude = rep(-35.0, 5),
        longitude = rep(NA_real_, 5)
    )
    
    expect_error(check_weather(data), "Longitude column contains only NA values")
})
