# load test data from the package
data(laventa)

# pick age data from different levels and thus different ages
nonisochronIndex <- which(laventa$elevation == 56.4 | laventa$elevation == 675.0)

# pick age data from the same age although from different samples
isochronIndex <- which(laventa$sample == "JG-R 89-2" | laventa$sample == "JG-R 88-2")
dataset <- laventa[isochronIndex, ]
# remove data with huge variances
dataset <- dataset[c(-21, -23), ]

test_that("mswd rejects the null isochron hypothesis", {
    expect_lt(mswd.test(age = laventa$age[nonisochronIndex], sd = laventa$one_sigma[nonisochronIndex]), 0.05)
})

test_that("mswd does not reject the null isochron hypothesis", {
    expect_gt(mswd.test(age = dataset$age, sd = dataset$one_sigma), 0.05)
})

test_that("age is not a numeric vector", {
    expect_error(mswd.test(age = letters[10], sd = rnorm(10)), "age is not a numeric vector", ignore.case = TRUE)
})

test_that("sd is not a numeric vector", {
    expect_error(mswd.test(age = rnorm(10), sd = letters[10]), "sd is not a numeric vector", ignore.case = TRUE)
})

test_that("output is a numeric vector", {
    expect_type(mswd.test(age = laventa$age[nonisochronIndex], sd = laventa$one_sigma[nonisochronIndex]), "double")
})

test_that("output is of length 1", {
    expect_length(mswd.test(age = laventa$age[nonisochronIndex], sd = laventa$one_sigma[nonisochronIndex]), 1)
})
