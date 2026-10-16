test_that("wang_engel_tt returns the curve as a data frame", {
    res <- wang_engel_tt(0, 25, 35)
    expect_s3_class(res, "data.frame")
    expect_named(res, c("x", "y"))
    expect_equal(nrow(res), 101)
    expect_equal(range(res$x), c(0, 35))
    expect_equal(nrow(wang_engel_tt(0, 25, 35, n = 36)), 36)
})

test_that("wang_engel_tt has zero response at min and max and peaks at opt", {
    res <- wang_engel_tt(0, 25, 35, n = 36)
    expect_equal(res$y[res$x == 0], 0)
    expect_equal(res$y[res$x == 35], 0, tolerance = 1e-12)
    expect_equal(res$x[which.max(res$y)], 25)
    expect_equal(max(res$y), 25)
    expect_true(all(res$y >= -1e-12))
})

test_that("wang_engel_tt matches the Wang-Engel equation", {
    res <- wang_engel_tt(0, 25, 35, n = 8)
    p <- log(2) / log(35 / 25)
    expected <- (2 * res$x ^ p * 25 ^ p - res$x ^ (2 * p)) / 25 ^ (2 * p) * 25
    expect_equal(res$y, expected)
    expect_equal(res$y[res$x == 10], 0.2799376 * 25, tolerance = 1e-6)
})

test_that("wang_engel_tt applies beta", {
    r1 <- wang_engel_tt(0, 25, 35, beta = 1, n = 8)
    r2 <- wang_engel_tt(0, 25, 35, beta = 0.5, n = 8)
    expect_equal(r2$y, (r1$y / 25) ^ 0.5 * 25)
})

test_that("wang_engel_tt validates inputs", {
    expect_error(wang_engel_tt(0, 25, "a"), "single numeric")
    expect_error(wang_engel_tt(0, c(20, 25), 35), "single numeric")
    expect_error(wang_engel_tt(0, NA_real_, 35), "single numeric")
    expect_error(wang_engel_tt(25, 0, 35), "min_temp < opt_temp < max_temp")
    expect_error(wang_engel_tt(0, 35, 35), "min_temp < opt_temp < max_temp")
    expect_error(wang_engel_tt(0, 25, 35, n = 1), "at least 2")
    expect_error(wang_engel_tt(0, 25, 35, beta = -0.1), "beta must be between 0 and 1")
    expect_error(wang_engel_tt(0, 25, 35, beta = 1.1), "beta must be between 0 and 1")
})
