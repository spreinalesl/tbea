
test_that("fasta2nexus fails with a deprecation message", {
    expect_error(fasta2nexus(),
                 regexp = "fasta2nexus was deprecated",
                 ignore.case = TRUE)
})
