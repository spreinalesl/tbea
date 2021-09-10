# set seed
set.seed(1453)

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

# check that find params works with the normal distribution
r_mean <- runif(n = 1, min = -4, max = 4)
r_sd <- runif(n = 1, min = 1, max = 10)
r_q <- c(0.025, 0.5, 0.975)
r_p <- pnorm(q = r_q, mean = r_mean, sd = r_sd)

test_that("findParams finds an arbitrary mean for the normal distribution", {
    expect_equal(findParams(q = r_q,
                            p = r_p,
                            output = "complete",
                            pdfunction = "pnorm",
                            params = c("mean", "sd"),
                            initVals = c(r_mean, r_sd))$par[1],
                 r_mean)
})

test_that("findParams finds an arbitrary sd for the normal distribution", {
    expect_equal(findParams(q = r_q,
                            p = r_p,
                            output = "complete",
                            pdfunction = "pnorm",
                            params = c("mean", "sd"),
                            initVals = c(r_mean, r_sd))$par[2],
                 r_sd)
})

# check that find params works with the poisson distribution
r_lambda <- runif(n = 1, min = 0, max = 5)
r_q <- c(0.025, 0.5, 0.975)
r_p <- ppois(q = r_q, lambda = r_lambda)

test_that("findParams finds an arbitrary lambda for the poisson  distribution", {
    expect_equal(findParams(q = r_q,
                            p = r_p,
                            output = "complete",
                            pdfunction = "ppois",
                            params = c("lambda"),
                            initVals = c(r_lambda))$par[1],
                 r_lambda)
})
