test_that("Thermal time", {
    old <- options(digits = 20)
    # The daily thermal time
    mint <- c(0, 10)
    maxt <- c(30, 40)
    x_temp <- c(0, 20, 35)
    y_temp <- c(0, 20, 0)
    res <- thermal_time(mint, maxt, x_temp, y_temp)
    expect_equal(res, c(15, 13.3333333333), tolerance=1e-3)
    res <- thermal_time(mint, maxt, x_temp, y_temp, method = '3hr')
    expect_equal(res, c(9.279687499999, 8.7905625000000), tolerance=1e-3)
    
    #expect_equal(res$value[1], 16.391951095089993, tolerance=1e-3)
    # Read weather records
    met_file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    records <- read_weather(met_file)
    x_temp <- c(0, 26, 34)
    y_temp <- c(0, 26, 0)
    res <- thermal_time(records$mint, records$maxt, x_temp, y_temp)
    expect_equal(round(res[1], 1), 21.6, tolerance=1e-3)
    res <- thermal_time(records$mint, records$maxt, x_temp, y_temp, method = "3hr")
    expect_equal(round(res[1], 1), 19.6, tolerance=1e-3)

    res <- thermal_time(
        records$mint,
        records$maxt,
        x_temp,
        y_temp,
        method = "HourlySinPpAdjusted",
        lat = records$latitude[1],
        date = records$date
    )
    expect_equal(length(res), length(records$mint))
    expect_true(all(is.finite(res)))
    
    options(old)
})

test_that("thermal_time HourlySinPpAdjusted matches mean hourly response", {
    mint <- c(10, 11, 12)
    maxt <- c(30, 31, 32)
    x_temp <- c(-50, 60)
    y_temp <- c(-50, 60)
    date <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))

    expected <- interpolate_hourly_sin_pp_adjusted(mint, maxt, lat = -27.5, date = date)
    result <- thermal_time(
        mint,
        maxt,
        x_temp,
        y_temp,
        method = "HourlySinPpAdjusted",
        lat = -27.5,
        date = date
    )

    expect_equal(result, expected, tolerance = 1e-8)
})

test_that("interpolate_hourly_sin_pp_adjusted returns daily means", {
    mint <- c(0, 10, 5)
    maxt <- c(30, 40, 35)
    date <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
    result <- interpolate_hourly_sin_pp_adjusted(mint, maxt, lat = -27.5, date = date)

    expect_type(result, "double")
    expect_equal(length(result), 3)
})

test_that("interpolate_hourly_sin_pp_adjusted handles APSIM boundaries", {
    mint <- c(10, 10, 10)
    maxt <- c(30, 30, 30)
    date <- as.Date(c("2020-01-01", "2020-01-01", "2020-01-01"))
    result <- interpolate_hourly_sin_pp_adjusted(mint, maxt, lat = -27.5, date = date)

    expect_equal(result[1], result[2], tolerance = 1e-8)
    expect_equal(result[2], result[3], tolerance = 1e-8)
})

test_that("interpolate_hourly_sin_pp_adjusted returns hourly matrix when hourly = TRUE", {
    mint <- c(0, 10, 5)
    maxt <- c(30, 40, 35)
    date <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
    hourly <- interpolate_hourly_sin_pp_adjusted(mint, maxt, date = date, lat = -27.5, hourly = TRUE)
    daily <- interpolate_hourly_sin_pp_adjusted(mint, maxt, date = date, lat = -27.5)
    expect_true(is.data.frame(hourly))
    expect_equal(dim(hourly), c(72, 6))
    expect_equal(round(daily, 1), c(16.7, 24.1, 20.1))
})

test_that("interpolate_hourly_sin_pp_adjusted requires scalar lat", {
    mint <- c(0, 10)
    maxt <- c(30, 40)
    date <- as.Date(c("2020-01-01", "2020-01-02"))
    expect_error(interpolate_hourly_sin_pp_adjusted(mint, maxt, date = date, lat = c(-27.5, -28)))
    expect_error(interpolate_hourly_sin_pp_adjusted(mint, maxt, date = date, lat = "not numeric"))
    expect_error(interpolate_hourly_sin_pp_adjusted(mint, maxt, date = date, lat = -27.4, hourly = "not logical"))
    expect_error(interpolate_hourly_sin_pp_adjusted(c(10), maxt, date = date, lat = c(-27.5, -28)))
    expect_error(interpolate_hourly_sin_pp_adjusted(c(10, NA), maxt, date = date, lat = c(-27.5, -28)))
    expect_error(interpolate_hourly_sin_pp_adjusted(c(10, 50), maxt, date = date, lat = c(-27.5, -28)))
    expect_error(interpolate_hourly_sin_pp_adjusted(c(10, 10, 10, 10), maxt, date = date, lat = c(-27.5, -28)))
})

test_that("thermal_time rejects NA values", {
    # Setup valid parameters
    x_temp <- c(0, 20, 35)
    y_temp <- c(0, 20, 0)
    
    # NA in mint
    mint_na <- c(0, NA, 10)
    maxt_valid <- c(30, 40, 35)
    expect_error(thermal_time(mint_na, maxt_valid, x_temp, y_temp))
    expect_error(thermal_time(mint_na, maxt_valid, x_temp, y_temp, method = "3hr"))
    
    # NA in maxt
    mint_valid <- c(0, 10, 15)
    maxt_na <- c(30, NA, 35)
    expect_error(thermal_time(mint_valid, maxt_na, x_temp, y_temp))
    expect_error(thermal_time(mint_valid, maxt_na, x_temp, y_temp, method = "3hr"))
    
    # NA in both
    expect_error(thermal_time(mint_na, maxt_na, x_temp, y_temp))
    expect_error(thermal_time(mint_na, maxt_na, x_temp, y_temp, method = "3hr"))
    
    # NA in x_temp
    mint <- c(0, 10)
    maxt <- c(30, 40)
    x_temp_na <- c(0, NA, 35)
    expect_error(thermal_time(mint, maxt, x_temp_na, y_temp))
    expect_error(thermal_time(mint, maxt, x_temp_na, y_temp, method = "3hr"))
    
    # NA in y_temp
    y_temp_na <- c(0, 20, NA)
    expect_error(thermal_time(mint, maxt, x_temp, y_temp_na))
    expect_error(thermal_time(mint, maxt, x_temp, y_temp_na, method = "3hr"))
    
    # NA in both x_temp and y_temp
    expect_error(thermal_time(mint, maxt, x_temp_na, y_temp_na))
    expect_error(thermal_time(mint, maxt, x_temp_na, y_temp_na, method = "3hr"))
})

test_that("thermal_time rejects mint > maxt", {
    # Setup valid parameters
    x_temp <- c(0, 20, 35)
    y_temp <- c(0, 20, 0)
    
    # Single case where mint > maxt
    mint <- c(0, 30, 10)
    maxt <- c(30, 20, 35)  # Second value: mint (30) > maxt (20)
    expect_error(thermal_time(mint, maxt, x_temp, y_temp))
    expect_error(thermal_time(mint, maxt, x_temp, y_temp, method = "3hr"))
    
    # All values where mint > maxt
    mint <- c(30, 40)
    maxt <- c(20, 30)
    expect_error(thermal_time(mint, maxt, x_temp, y_temp))
    expect_error(thermal_time(mint, maxt, x_temp, y_temp, method = "3hr"))
})

test_that("interpolate_3hr returns correct matrix dimensions", {
    mint <- c(0, 10, 5)
    maxt <- c(30, 40, 35)
    result <- interpolate_3hr(mint, maxt)
    
    # Check that result is a matrix
    expect_type(result, "double")
    expect_true("matrix" %in% class(result))
    expect_equal(dim(result), c(3, 8))  # 3 rows for 3 days, 8 columns for 3-hourly intervals
    # Check dimensions: 3 rows (for 3 days) and 8 columns (for 8 3-hourly intervals)
    expect_equal(dim(result), c(3, 8))
})

test_that("interpolate_3hr requires numeric vectors", {
    # Non-numeric mint
    expect_error(interpolate_3hr(c("0", "10"), c(30, 40)))
    # Non-numeric maxt
    expect_error(interpolate_3hr(c(0, 10), c("30", "40")))
})

test_that("interpolate_3hr requires equal length vectors", {
    mint <- c(0, 10)
    maxt <- c(30, 40, 50)  # Different length
    expect_error(interpolate_3hr(mint, maxt))
})

test_that("interpolate_3hr values are bounded by mint and maxt", {
    mint <- c(5, 10, 15)
    maxt <- c(25, 30, 35)
    result <- interpolate_3hr(mint, maxt)
    
    # Check each row: all values should be >= mint and <= maxt
    for (i in 1:nrow(result)) {
        expect_true(all(result[i, ] >= mint[i]))
        expect_true(all(result[i, ] <= maxt[i]))
    }
})

test_that("interpolate_3hr with single day", {
    mint <- 10
    maxt <- 30
    result <- interpolate_3hr(mint, maxt)
    
    expect_equal(dim(result), c(1, 8))
    # Values should be between mint and maxt
    expect_true(all(result >= mint))
    expect_true(all(result <= maxt))
})

test_that("interpolate_3hr produces expected values", {
    mint <- c(0, 10)
    maxt <- c(30, 40)
    result <- interpolate_3hr(mint, maxt)
    
    # From the function documentation example
    # Expected result based on the sine curve formula
    # First row should have values between 0 and 30
    # Second row should have values between 10 and 40
    expect_equal(round(result[1, 1], 3), round(0 + (30 - 0) * (0.92105 + 0.1140*1 - 0.0703*1*1 + 0.0053*1*1*1), 3))
    expect_equal(round(result[2, 1], 3), round(10 + (40 - 10) * (0.92105 + 0.1140*1 - 0.0703*1*1 + 0.0053*1*1*1), 3))
})

test_that("interpolate_3hr rejects NA values", {
    # NA in mint
    mint_na <- c(0, NA, 10)
    maxt_valid <- c(30, 40, 35)
    expect_error(interpolate_3hr(mint_na, maxt_valid))
    
    # NA in maxt
    mint_valid <- c(0, 10, 15)
    maxt_na <- c(30, NA, 35)
    expect_error(interpolate_3hr(mint_valid, maxt_na))
    
    # NA in both
    expect_error(interpolate_3hr(mint_na, maxt_na))
})

test_that("interpolate_3hr rejects mint > maxt", {
    # Single case where mint > maxt
    mint <- c(0, 30, 10)
    maxt <- c(30, 20, 35)  # Second value: mint (30) > maxt (20)
    expect_error(interpolate_3hr(mint, maxt))
    
    # All values where mint > maxt
    mint <- c(30, 40)
    maxt <- c(20, 30)
    expect_error(interpolate_3hr(mint, maxt))
})
