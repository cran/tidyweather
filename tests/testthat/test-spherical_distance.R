test_that("spherical_distance returns zero for identical coordinates", {
    expect_equal(spherical_distance(-35.0, 147.0, -35.0, 147.0), 0)
})

test_that("spherical_distance matches a known great-circle distance", {
    distance <- spherical_distance(34.05, -118.25, 40.7128, -74.0060)

    expect_equal(distance, 3936.38, tolerance = 0.1)
})

test_that("spherical_distance is symmetric", {
    forward <- spherical_distance(-35.2809, 149.13, -33.8688, 151.2093)
    reverse <- spherical_distance(-33.8688, 151.2093, -35.2809, 149.13)

    expect_equal(forward, reverse)
})

test_that("spherical_distance handles vectorized inputs", {
    lat1 <- c(-35.0)
    lon1 <- c(147.0)
    lat2 <- c(-35.0, -35.2809)
    lon2 <- c(147.0, 149.13)

    distances <- spherical_distance(lat1, lon1, lat2, lon2)

    expect_equal(length(distances), 2)
    expect_equal(distances[1], 0)
    expect_equal(distances[2], 196.2, tolerance = 0.1)
})

test_that("spherical_distance throws errors for invalid inputs", {
    expect_error(spherical_distance("not a number", 147.0, -35.0, 147.0))
    expect_error(spherical_distance(-35.0, "not a number", -35.0, 147.0))
    expect_error(spherical_distance(-35.0, 147.0, "not a number", 147.0))
    expect_error(spherical_distance(-35.0, 147.0, -35.0, "not a number"))
    expect_error(spherical_distance(c(-35.0, -33.8688), c(147.0), c(-35.0, -35.2809), c(147.0, 149.13)))
    expect_error(spherical_distance(c(-95), c(147.0), c(-35.0), c(147.0)))
    expect_error(spherical_distance(c(-35.0), c(190), c(-35.0), c(147.0)))
    expect_error(spherical_distance(c(-35.0, -33.8688), c(190, 151.2093), c(-35.0, -35.2809), c(147.0, 149.13)))
})
