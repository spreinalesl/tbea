# create some input tree and their expected outputs in the tempdir()
setwd(tempdir())

# create a file with multiple trees in TNT format to convert to newick format
writeLines(
    text = c(
        "tread \'some comment\'",
        "(Taxon_A ((Taxon_B Taxon_C)(Taxon_D Taxon_E)))*",
        "(Taxon_A (Taxon_B (Taxon_C (Taxon_D Taxon_E))))*",
        "(Taxon_A (Taxon_C (Taxon_B (Taxon_D Taxon_E))));",
        "proc-;"),
    con = "someTrees.tre"
)

# create a file with multiple trees newick format to convert
writeLines(
    text = c(
        "(Taxon_A,((Taxon_B,Taxon_C),(Taxon_D,Taxon_E)));", 
        "(Taxon_A,(Taxon_B,(Taxon_C,(Taxon_D,Taxon_E))));", 
        "(Taxon_A,(Taxon_C,(Taxon_B,(Taxon_D,Taxon_E))));"),
    con = "someTrees.newick"
)



#file, output = NULL, string = NULL, return = FALSE, subsetting = FALSE, name.sep = NULL

test_that("input file does not exist while string is NULL", {
    expect_error(tnt2newick(file = "file_that_does_not_exist.tre", output = NULL, string = NULL, return = FALSE, subsetting = FALSE, name.sep = NULL),
                 regexp = "does not exist",
                 ignore.case = TRUE)
})

test_that("output is NULL and return is FALSE", {
    expect_error(tnt2newick(file = "someTrees.tre", output = NULL, string = NULL, return = FALSE, subsetting = FALSE, name.sep = NULL),
                 regexp = "output file is required",
                 ignore.case = TRUE)
})

test_that("output file and return can not be in effect at the same time", {
    expect_warning(tnt2newick(file = "someTrees.tre", output = "outputTrees.newick", string = NULL, return = TRUE, subsetting = FALSE, name.sep = NULL),
                 regexp = "are in use when only one of them should be",
                 ignore.case = TRUE)
})

test_that("output file already exists", {
    expect_warning(tnt2newick(file = "someTrees.tre", output = "someTrees.newick", string = NULL, return = FALSE, subsetting = FALSE, name.sep = NULL),
                 regexp = "already exists, it will be overwritten",
                 ignore.case = TRUE)
})

test_that("number of name separators got completely replaced using name.sep", {
    expect_equal(sum(sapply(gregexpr(pattern = "_", text = readLines(con = "someTrees.tre"), fixed=TRUE), function(i) sum(i > 0))),
                 sum(sapply(gregexpr(pattern = "--", text = tnt2newick(file = "someTrees.tre", output = NULL, string = NULL, return = TRUE, subsetting = FALSE, name.sep = c("_", "--")), fixed=TRUE), function(i) sum(i > 0))))
})

# clean testing files
file.remove(dir(pattern = ".tre$"))
file.remove(dir(pattern = ".newick$"))
