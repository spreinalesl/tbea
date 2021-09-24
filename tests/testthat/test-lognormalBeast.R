test_that("The parameter \'from\' has not been defined", {
    expect_error(lognormalBeast(M = 2, S = 2, meanInRealSpace = TRUE, offset = 0, to = 20, by = 0.05), "must be defined", ignore.case = TRUE)
})

test_that("The parameter \'to\' has not been defined", {
    expect_error(lognormalBeast(M = 2, S = 2, meanInRealSpace = TRUE, offset = 0, from = 10, by = 0.05), "must be defined", ignore.case = TRUE)
})

test_that("The parameter \'S\' is improper", {
    expect_error(lognormalBeast(M = 2, S = -1, meanInRealSpace = TRUE, offset = 0, from = 0, to = 5, by = 0.05), "must be a positive number higher than zero", ignore.case = TRUE)
})

test_that("The parameter \'M\' is improper", {
    expect_error(lognormalBeast(M = -1, S = 2, meanInRealSpace = TRUE, offset = 0, from = 0, to = 5, by = 0.05), "must be a positive number higher than zero", ignore.case = TRUE)
})
