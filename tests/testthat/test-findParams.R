# check input quantiles and percentiles vectors of the same length
test_that("error if length of vectors p and q are not the same", {
    expect_error(findParams(q = c(-1, 0, 1), p = c(0.25, 0.75)), "length of p and q must be equal", ignore.case = TRUE)
})

# check that omitting initVals throws a warning on usage
test_that("omitting the initVal parameter throws a warning", {
    expect_warning(findParams(q = c(-1.959964, 0.000000, 1.959964),
                              p = c(0.025, 0.50, 0.975),
                              output = "complete",
                              pdfunction = "pnorm",
                              params = c("mean", "sd")),
                   "initVals not provided", ignore.case = TRUE)
})


