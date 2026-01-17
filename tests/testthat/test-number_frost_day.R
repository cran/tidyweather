test_that("number_frost_day works with basic input", {
    # Create test data with known frost pattern
    test_data <- tibble::tibble(
        day = 1:365,
        mint = c(rep(-2, 50), rep(3, 100), rep(-1, 1), rep(8, 214)) # 51 frost days total
    )

    result <- number_frost_day(test_data)
    expect_equal(result$number_frost_days[1], 51)
})

test_that("number_frost_day handles no frost scenario", {
    # Test data with no frost (all temperatures above threshold)
    test_data <- tibble::tibble(
        day = 1:365,
        mint = rep(5, 365)
    )

    result <- number_frost_day(test_data)
    expect_equal(result$number_frost_days[1], 0)
})

test_that("number_frost_day respects threshold parameter", {
    # Test with custom threshold
    test_data <- tibble::tibble(
        day = 1:365,
        mint = c(rep(1, 100), rep(-3, 1), 1, rep(5, 263)) # Different frost counts based on threshold
    )

    # With default threshold (0), should count days <= 0
    result_default <- number_frost_day(test_data)
    expect_equal(result_default$number_frost_days[1], 1)

    # With threshold 2, should count days <= 2
    result_custom <- number_frost_day(test_data, threshold = 2)
    expect_equal(result_custom$number_frost_days[1], 102)

    expect_error(number_frost_day(test_data, threshold = "high"))
    expect_error(number_frost_day(test_data, threshold = c(1, 2)))
})

test_that("number_frost_day validates data length with require_full_year", {
    # Test with insufficient data when require_full_year = TRUE
    short_data <- tibble::tibble(
        day = 1:200,
        mint = rep(-1, 200)
    )

    expect_error(
        result <- number_frost_day(short_data, require_full_year = TRUE)
    )
})

test_that("number_frost_day works with partial year when require_full_year = FALSE", {
    # Test with partial year data
    partial_data <- tibble::tibble(
        day = 1:100,
        mint = c(rep(-2, 30), rep(5, 69), rep(-1, 1)) # 31 frost days
    )

    result <- number_frost_day(partial_data, require_full_year = FALSE)
    expect_equal(result$number_frost_days[1], 31)
})


test_that("number_frost_day requires mint column", {
    # Test with data missing 'mint' column
    wrong_data <- tibble::tibble(
        day = 1:365,
        temperature = rep(-1, 365)
    )

    expect_error(
        number_frost_day(wrong_data),
        "Input data should be a column mint for daily minimum temperatures"
    )
})

test_that("number_frost_day works with leap year data", {
    # Test with 366 days (leap year)
    leap_data <- tibble::tibble(
        day = 1:366,
        mint = c(rep(-2, 60), rep(5, 305), rep(-1, 1)) # 61 frost days
    )

    result <- number_frost_day(leap_data)
    expect_equal(result$number_frost_days[1], 61)
})

test_that("Weather records integration test", {
    # Integration test with real weather data
    file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    expect_true(file.exists(file))

    records <- read_weather(file)

    # Test with 2024 data
    result <- records |>
        dplyr::filter(year == 2024) |>
        number_frost_day(require_full_year = FALSE)

    # Should return a count of frost days
    expect_equal(result$number_frost_days[1], 29)
    expect_error({
        result <- records |>
            dplyr::group_by(year) |> 
            number_frost_day(require_full_year = TRUE)
    })
    result <- records |>
        dplyr::group_by(year) |> 
        number_frost_day(require_full_year = FALSE)
    expect_equal(result$number_frost_days, c(0, 29, 9))
})
