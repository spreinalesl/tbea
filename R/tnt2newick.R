#' tnt2newick: Function for converting from TNT tree format to newick parenthetical format
#' 
#' @param file A vector of type 'character' with the path to the original TNT tree file.
#'
#' @param output A vector of type 'character' with the path to output files to contain the tree in newick format.
#'
#' @param subsetting A vector of type 'logical' indicating whether subsetting (i.e., chopping at once the first and last line of the TNT tree file) should be done. Otherwise, explicit text replacements removing such lines are used.
#' 
#' @return This function writes to the disk a text file containing the tree converted to newick format.
#'
#' @author Gustavo A. Ballen
#'
#' @details This function has been tested for cases where only one tree is in the original tnt tree file. Please be careful with files containing multiple trees.
#' 
#' @examples
#' # Convert a tree in TNT tree format to newick format
#' \dontrun{
#' tnt2newick(file = "my_TNT_tree.tre", output = "my_TNT_tree.newick")
#' }
#' @export

tnt2newick <- function(file, output, subsetting = TRUE){
    tree <- readLines(file)
    if(subsetting) {
        tree <- tree[-c(1,length(tree))]
    } else {
        tree <- gsub(pattern = "tread.*", replacement = "", x = tree, ignore.case = TRUE)
        tree <- gsub(pattern = "proc-;.*", replacement = "", x = tree, ignore.case = TRUE)
        tree <- gsub(pattern = "proc/.*", replacement = "", x = tree, ignore.case = TRUE)
    }

    tree <- gsub(pattern = "*", replacement = ";", x = tree, fixed = TRUE)
    tree <- gsub(pattern = " ", replacement = ",", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ")(", replacement = "),(", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ",)", replacement = ")", x = tree, fixed = TRUE)
    tree <- gsub(pattern = "_", replacement = " ", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ",=", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = "=", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = " =", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = "=", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ":/", replacement = "", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ":", replacement = " ", x = tree, fixed = TRUE)
    if(length(which(tree == "")) == 0) {
        writeLines(text = tree, con = output)
    } else {
        tree <- tree[-which(tree == "")]
        writeLines(text = tree, con = output)
    }
}
