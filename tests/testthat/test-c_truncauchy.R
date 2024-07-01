set.seed(1517)
tolerance = 1e-2

testValues.tr <- c(4.93, 12.12, 24.43, 49.20)
true_c <- c(0.2, 0.5, 1, 2)
# the values below should be approx. c = 0.2, 0.5, 1, 2
# according to the paml documentation
fitted_c <- c()
for (i in testValues.tr) {
    fitted_c <- c(fitted_c, c_truncauchy(tl=1, tr=i, p=0.1, pr=0.975, al=0.025))
}
test_that("c_truncauchy finds an c as in the PAML documentation", {
    expect_equal(true_c,
                 fitted_c,
                 tolerance=tolerance)
})
