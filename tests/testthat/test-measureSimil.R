# set seed
set.seed(1507)
n <- 10000000
splits <- 1000000
# this method uses a quite rough integration technique and requires a large number of splits in order to have an accuracy large enough as to pass the test. Thus, 1e-3 is used only for testing with moderate time. With larger splits the tests take a lot of time. 
tolerance = 1e-2

test_that("two identical distributions have similarity = 1", {
    expect_equal(measureSimil(d1 = rnorm(n, mean = 0, 1),
                              d2 = rnorm(n, mean = 0, 1),
                              plot = FALSE, splits = splits),
                 1.0, tolerance = tolerance)
})
    
test_that("two different yet partially overlapping distributions have similarity greater than 0", {
    expect_gt(measureSimil(d1 = rnorm(n, mean = 3, 1),
                           d2 = rnorm(n, mean = 0, 1),
                           plot = FALSE, splits = splits),
              0.0)
})

test_that("two different yet partially overlapping distributions have similarity less than 1", {
    expect_lt(measureSimil(d1 = rnorm(n, mean = 3, 1),
                           d2 = rnorm(n, mean = 0, 1),
                           plot = FALSE, splits = splits),
              1.0)
})

test_that("two non-overlapping distributions have similarity = 0.0", {
    expect_equal(measureSimil(d1 = rnorm(n, mean = 8, 1),
                           d2 = rnorm(n, mean = 0, 1),
                           plot = FALSE, splits = splits),
              0.0, tolerance = tolerance)
})
