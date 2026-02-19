test_that("weather_options() returns all options when called with no arguments", {
    # Reset to ensure clean state
    weather_options$reset()

    options <- weather_options$get()
    expect_type(options, "list")
    expect_true("extreme" %in% names(options))
    expect_true("require_full_year" %in% names(options))
    expect_true("frost_threshold" %in% names(options$extreme))
    
    expect_equal(options$extreme$frost_threshold, 0)
})



test_that("weather_options() can set and get match the original type and length", {
    # Reset to ensure clean state
    weather_options$reset()

    expect_no_error(weather_options$set("require_full_year" = FALSE))
    expect_no_error(weather_options$set("extreme.frost_threshold" = 2))
    expect_error(weather_options$set("require_full_year" = "false"))
    expect_error(weather_options$set("extreme.frost_threshold" = "false"))    
    expect_error(weather_options$set("require_full_year" = c(TRUE, FALSE)))
    expect_error(weather_options$set("extreme.frost_threshold" = c(1, 2)))
})

test_that("weather_options() can set and get first level options", {
    # Reset to ensure clean state
    weather_options$reset()

    # Set frost_threshold
    weather_options$set("require_full_year" = FALSE)

    # Verify it was set
    options <- weather_options$get()
    expect_equal(options$require_full_year, FALSE)
})


test_that("weather_options() can set and get frost_threshold", {
    # Reset to ensure clean state
    weather_options$reset()

    # Set frost_threshold
    weather_options$set("extreme.frost_threshold" = 2)

    # Verify it was set
    options <- weather_options$get()
    expect_equal(options$extreme$frost_threshold, 2)

    # Set to negative value
    weather_options$set("extreme.frost_threshold" = -5)
    options <- weather_options$get()
    expect_equal(options$extreme$frost_threshold, -5)

    # Set to decimal value
    weather_options$set("extreme.frost_threshold" = 1.5)
    options <- weather_options$get()
    expect_equal(options$extreme$frost_threshold, 1.5)
})

test_that("weather_options() validates argument names", {
    # Reset to ensure clean state
    weather_options$reset()

    # Unnamed arguments should fail
    expect_error(
        weather_options$set(2),
        "All arguments must be named"
    )

    # Mixed named and unnamed should fail
    expect_error(
        weather_options$set("extreme.frost_threshold" = 2, 5),
        "All arguments must be named"
    )
})

test_that("weather_options() validates option keys", {
    # Reset to ensure clean state
    weather_options$reset()

    # Invalid group should fail
    expect_error(
        weather_options$set("invalid.frost_threshold" = 2)
    )

    # Invalid key format (too many dots) should fail
    expect_error(
        weather_options$set("extreme.frost.threshold" = 2)
    )

    # Invalid key format (no dots for nested) - this should fail
    expect_error(
        weather_options$set("invalid_top_level" = 2)
    )
})

test_that("weather_options() handles multiple options at once", {
    # Reset to ensure clean state
    weather_options$reset()

    # Set multiple nested options (when more are available)
    weather_options$set("extreme.frost_threshold" = 3)

    # Verify all were set
    options <- weather_options$get()
    expect_equal(options$extreme$frost_threshold, 3)
})

test_that("weather_reset() resets options to defaults", {
    # Set some non-default values
    weather_options$set("extreme.frost_threshold" = 10)

    # Verify they were changed
    options <- weather_options$get()
    expect_equal(options$extreme$frost_threshold, 10)

    # Reset
    weather_options$reset()

    # Verify they're back to defaults
    options <- weather_options$get()
    expect_equal(options$extreme$frost_threshold, 0)
})

test_that("weather_options() preserves other settings when updating one", {
    # Reset to ensure clean state
    weather_options$reset()

    # Get initial state
    initial_options <- weather_options$get()

    # Change frost_threshold
    weather_options$set("extreme.frost_threshold" = 5)

    # Verify only frost_threshold changed
    new_options <- weather_options$get()
    expect_equal(new_options$extreme$frost_threshold, 5)

    # All other options should remain the same
    initial_options$extreme$frost_threshold <- 5
    expect_equal(new_options, initial_options)
})

test_that("weather_options() works with different data types", {
    # Reset to ensure clean state
    weather_options$reset()

    # Test with integer
    weather_options$set("extreme.frost_threshold" = 2)
    expect_equal(weather_options$get()$extreme$frost_threshold, 2)

    # Test with numeric
    weather_options$set("extreme.frost_threshold" = 2.5)
    expect_equal(weather_options$get()$extreme$frost_threshold, 2.5)

    # Test with negative
    weather_options$set("extreme.frost_threshold" = -1.5)
    expect_equal(weather_options$get()$extreme$frost_threshold, -1.5)
})



test_that("weather_options() documentation examples work", {
    # Reset to ensure clean state
    weather_options$reset()

    # Test the examples from the documentation

    # Get all options
    all_options <- weather_options$get()
    expect_type(all_options, "list")

    # Set frost_threshold
    weather_options$set("extreme.frost_threshold" = 2)
    expect_equal(weather_options$get()$extreme$frost_threshold, 2)
})

# Clean up after all tests
test_that("cleanup after options tests", {
    weather_options$reset()
    expect_equal(weather_options$get()$extreme$frost_threshold, 0)
})

# Clean up after all tests
test_that("get_weather_option", {
    expect_silent(weather_options$reset())
})

