test_that("last_frost_day works with basic input", {
    # Create test data with known frost pattern
    test_data <- tibble::tibble(
        day = 1:365,
        mint = c(rep(-2, 50), rep(3, 100), rep(-1, 1), rep(8, 214)) # Last frost on day 151
    )

    result <- last_frost_day(test_data)
    expect_equal(result$last_frost_day[1], 151)
})

test_that("last_frost_day handles no frost scenario", {
    # Test data with no frost (all temperatures above threshold)
    test_data <- tibble::tibble(
        day = 1:365,
        mint = rep(5, 365)
    )

    result <- last_frost_day(test_data)
    expect_true(is.na(result$last_frost_day[1]))
})

test_that("last_frost_day respects threshold parameter", {
    # Test with custom threshold
    test_data <- tibble::tibble(
        day = 1:365,
        mint = c(rep(1, 100), rep(-3, 1), 1, rep(5, 263)) # Frost at day 101 with threshold 2
    )

    # With default threshold (0), day 101 should not be frost
    result_default <- last_frost_day(test_data)
    expect_equal(result_default$last_frost_day[1], 101)

    # With threshold 2, day 101 should be last frost
    result_custom <- last_frost_day(test_data, threshold = 2)
    expect_equal(result_custom$last_frost_day[1], 102)


    expect_error(last_frost_day(test_data, threshold = "high"))
    expect_error(last_frost_day(test_data, threshold = c(1, 2)))
})

test_that("last_frost_day validates data length with require_full_year", {
    # Test with insufficient data when require_full_year = TRUE
    short_data <- tibble::tibble(
        day = 1:200,
        mint = rep(-1, 200)
    )

    expect_error(
        result <- last_frost_day(short_data, require_full_year = TRUE)
    )
})

test_that("last_frost_day works with partial year when require_full_year = FALSE", {
    # Test with partial year data
    partial_data <- tibble::tibble(
        day = 1:100,
        mint = c(rep(-2, 30), rep(5, 69), rep(-1, 1)) # Last frost on day 100
    )

    result <- last_frost_day(partial_data, require_full_year = FALSE)
    expect_equal(result$last_frost_day[1], 100)
})


test_that("last_frost_day validates hemisphere parameter", {
    test_data <- tibble::tibble(
        day = 1:365,
        mint = c(rep(-2, 100), rep(5, 265))
    )

    # Test invalid hemisphere
    expect_error(
        last_frost_day(test_data, hemisphere = "invalid")
    )

    # Test northern hemisphere (not implemented)
    expect_error(
        last_frost_day(test_data, hemisphere = "north")
    )
})

test_that("last_frost_day requires mint column", {
    # Test with data missing 'mint' column
    wrong_data <- tibble::tibble(
        day = 1:365,
        temperature = rep(-1, 365)
    )

    expect_error(
        last_frost_day(wrong_data),
        "Input data should be a column mint for daily minimum temperatures"
    )
})

test_that("last_frost_day works with leap year data", {
    # Test with 366 days (leap year)
    leap_data <- tibble::tibble(
        day = 1:366,
        mint = c(rep(-2, 60), rep(5, 305), rep(-1, 1)) # Last frost on day 366
    )

    result <- last_frost_day(leap_data)
    expect_equal(result$last_frost_day[1], 366)
})

test_that("Weather records integration test", {
    # Integration test with real weather data
    file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    expect_true(file.exists(file))

    records <- read_weather(file)

    # Test with 2024 data
    result <- records |>
        dplyr::filter(year == 2024) |>
        last_frost_day(require_full_year = FALSE)

    # Should return either an integer day number or NA
    expect_equal(result$last_frost_day[1], 262)
    expect_error({
        result <- records |>
            dplyr::group_by(year) |> 
            last_frost_day(require_full_year = TRUE)
    })
    result <- records |>
        dplyr::group_by(year) |> 
        last_frost_day(require_full_year = FALSE)
    expect_equal(result$last_frost_day, c(NA_integer_, 262, 183))

})
