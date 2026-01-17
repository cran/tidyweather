test_that("Weather records for apsim format", {
    expect_error(read_weather("non_exist_file.met"))
    expect_error(read_weather(c("file1.met", "file2.met")))
    expect_error(read_weather(1))

    file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
    expect_true(file.exists(file))

    expect_error(read_weather(file, format = "WRONGFORMAT"))
    expect_error(read_weather(file, format = c("WRONGFORMAT", "WRONGFORMAT2")))

    records <- read_weather(file)
    expect_true(all(c("date", "year", "day", "maxt", "mint", "radn", "rain") %in% names(records)))
    expect_equal(nrow(records), 587)
    expect_equal(records$year[1], 2023)
    expect_equal(records$date[1], as.Date("2023-12-01"))
    expect_equal(records$mint[1], 14.4)
})
