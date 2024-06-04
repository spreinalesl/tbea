#' tnt2newick: Function for converting from TNT tree format to newick parenthetical format
#' 
#' @param file A vector of type 'character' with the path to the original TNT tree file.
#'
#' @param output A vector of type 'character' with the path to output files to contain the tree in newick format.
#'
#' @param string A vector of type 'character' which can be either an object in memory or a string for interactive transformation, in TNT format. Use file in case your tree(s) are stored in a file instead.
#'
#' @param return A 'logical' expression indicating whether to write the newick tree(s) to a file in 'output' (if FALSE, the default), or whether to return to the screen (if TRUE), potentially to be stored in a vector via the '<-' operator.
#'
#' @param subsetting A vector of type 'logical' indicating whether subsetting (i.e., chopping at once the first and last line of the TNT tree file) should be done. Otherwise, explicit text replacements removing such lines are used. The default is false because it does not play well with multi-tree TNT files. Only use subsetting = TRUE if you are sure that there is only one tree in the file with the commands, tread and proc as first and last lines.
#'
#' @param name.sep A vector of length 2 and type 'character' for carrying out separator conversion in the names of terminals. For instance, if the terminals have names composed of two words separated by an underscore (_) and you want them to be separated by space ( ) then use name.sep = c("_", " "). This does not support regular expressions.
#' 
#' @return This function writes to the disk a text file containing the tree converted to newick format. Alternatively, it returns the output to the screen or writes it to an object in memory thanks to the argument 'string'.
#'
#' @author Gustavo A. Ballen
#'
#' @details This function has been tested for cases where only one tree is in the original tnt tree file. Please be careful with files containing multiple trees.
#' 
#' @examples
#' \dontrun{
#' tnt2newick(file = "someTrees.tre", return = TRUE)
#' }
#' @export

tnt2newick <- function(file, output = NULL, string = NULL, return = FALSE, subsetting = FALSE, name.sep = NULL){
    
    # check that input file exists or a tree string is provided
    if (!file.exists(file) & is.null(string)) {
        stop(paste("File ", file, " does not exist and \'string\'is NULL, you need either an input \'file\' or an object with trees in \'string\'", sep = ""))
    }

    # check that output objects or instructions are provided
    if (is.null(output) & !(return)) {
        stop("An output file is required when return = FALSE")
    }

    # both output and return can not be in effect at the same time
    if (!is.null(output) & return) {
        warning("Both \'output\' and \'return\' are in use when only one of them should be. The output will be returned and not written to a file. Provide the argument output with a path to the output file and set return = FALSE")
    }

    # warn about a pre-existing file
    if (!(is.null(output)) & !(return)) {
        if (file.exists(output)) {
            warning(paste("File ", output, " already exists, it will be overwritten", sep = ""))
        }
    }

    # check whether we are using a string or a file in the disk
    if (!is.null(string)) {
        tree <- string
    } else {
        tree <- readLines(file)
    }
    
    # use subsetting to remove tread and proc commands, probably not the safest choice
    if (subsetting) {
        tree <- tree[-c(1,length(tree))]
    } else {
        tree <- gsub(pattern = "tread.*", replacement = "", x = tree, ignore.case = TRUE)
        tree <- gsub(pattern = "proc-;.*", replacement = "", x = tree, ignore.case = TRUE)
        tree <- gsub(pattern = "proc/.*", replacement = "", x = tree, ignore.case = TRUE)
    }
    
    # string replacements
    tree <- gsub(pattern = "*", replacement = ";", x = tree, fixed = TRUE)
    tree <- gsub(pattern = " ", replacement = ",", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ")(", replacement = "),(", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ",)", replacement = ")", x = tree, fixed = TRUE)
    
    # If name separators are to be used, look for the first one and replace with the second one
    if (!is.null(name.sep)) {
        tree <- gsub(pattern = name.sep[1], replacement = name.sep[2], x = tree, fixed = TRUE)
    }

    # some more string replacements
    tree <- gsub(pattern = ",=", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = "=", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = " =", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = "=", replacement = ":", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ":/", replacement = "", x = tree, fixed = TRUE)
    tree <- gsub(pattern = ":", replacement = " ", x = tree, fixed = TRUE)

    # strip blank lines before returning or writing to file
    if (length(which(tree == "")) != 0) {
        tree <- tree[-which(tree == "")]
    }

    # either return to the screen or write to a file
    if (return) {
        return(tree)
    } else {
        writeLines(text = tree, con = output)
    }
}
