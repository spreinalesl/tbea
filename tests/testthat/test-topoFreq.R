trs <- ape::rmtree(n=4, N=10001, rooted=FALSE)

test_that("return a warning if maxtrees is larger than the number suggested", {
    expect_warning(topoFreq(mphy=trs))
})
