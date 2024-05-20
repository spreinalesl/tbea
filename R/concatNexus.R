#' concatNexus: Function for concatenation of nexus matrices both morphological and molecular
#' 
#' @param matrices A vector of type 'character' with paths to the nexus alignments or their file names. If \code{morphoFilename} is non-null, either the path to the morphological partition or its file name must be included too. The default is NULL and it must be defined if none of \code{pattern} and \code{path} are included.
#'
#' @param pattern A vector of type 'character' and length one containing the text pattern to identify the alignments of interest. It would be tipically be some suffix and/or file extension (see examples).
#'
#' @param path A vector of type 'character' and length one pointing to the directory where the matrices are located. It is used in combination with \code{pattern} in order to build a path to each matrix file (see examples).
#' 
#' @param filename A vector of type 'character' and length one with the file name (or path and file name) for the concatenated output matrix.
#'
#' @param morpho A vector of type 'logical' and length one indicating whether a morphological matrix is included in the concatenation.
#' 
#' @param morphoFilename A vector of type 'character' and length one with the file name or path to the morphological nexus matrix. Needed if \code{morpho = TRUE}.
#'
#' @param sumFilename A vector of type 'character' and length one with the file name or path to the summary information of partition start and end positions. Useful for specifying concatenated analyses in MrBayes where each partition in the matrix might have its own substitution model.
#'
#' @return This function writes to the disk two files, one with the concatenated matrix and one with the summary information on partition positions in the complete matrix.
#'
#' @author Gustavo A. Ballen
#'
#' @details This function will concatenate matrices in nexus format (mandatory) and write to the disk the output and summary information on the partitions. It requires that the input matrices all share the same taxa in the same positions.
#' 
#' @examples
#' # Concatenate all the matrices in a given path,
#' # ending with the pattern 'aligned.nex', including a morphological matrix
#' # also defined with a pattern
#' \dontrun{
#' path <- "sequences"
#' pattern <- "aligned.nex$"
#' 
#' concatNexus(matrices = NULL, pattern = pattern,
#'             filename = paste(path, "concatenatedMolmorph.nexus", sep = "/"),
#'             path = path, 
#'             morpho = TRUE,
#'             morphoFilename = paste(path, grep(pattern = "morfologia",
#'                                               x = dir(path, pattern), value = TRUE),
#'                                    sep = "/"),
#'             sumFilename = "partitions.txt")
#' }
#' # Concatenate arbitrary matrices in the working directory,
#' # including a morphological matrix, return a concatenated file in the same dir
#' \dontrun{
#' concatNexus(matrices = c("coi.nex", "rag1.nex", "cytb.nex", "morphology.nex"),
#'             filename = "concatenatedMolmorph.nexus", 
#'             morpho = TRUE,
#'             morphoFilename = "morphology.nex",
#'             sumFilename = "partitions.txt")
#' }
#' @export
#' @importFrom utils write.table

concatNexus <- function(matrices = NULL, pattern, path, filename, morpho = FALSE, morphoFilename = NULL, sumFilename) {
    NCHARregex <- "NCHAR[[:blank:]]*=[[:blank:]]*[[:digit:]]+"
    NTAXregex <- "NTAX[[:blank:]]*=[[:blank:]]*[[:digit:]]+"
    INTERLEAVEregex <- "INTERLEAVE[[:blank:]]*=[[:blank:]]*[[:alpha:]]+"
    DATATYPEregex <- "DATATYPE[[:blank:]]*=[[:blank:]]*[[:alpha:]]+"

    if (!is.null(matrices)) {
        nexFiles <- matrices
    } else {
        nexFiles <- dir(path = path, pattern = pattern)
        nexFiles <- paste(path,  nexFiles, sep = "/")
    }
    # if there is no morphological partition or no one is needed, chop out the filename
    if (!morpho & !is.null(morphoFilename)) {
        nexFiles <- nexFiles[-grep(pattern = morphoFilename, x = nexFiles)]
    }
    # reorder the filenames so that the morpho file comes last
    if (morpho) {
        if (is.null(morphoFilename)) {
            stop("morpho is set to TRUE but morphoFilename is NULL, a filename is necessary in this case\n")
        }
        choppedFilename <- nexFiles[grep(pattern = morphoFilename, x = nexFiles)]
        nexFiles <- nexFiles[-grep(pattern = morphoFilename, x = nexFiles)]
        nexFiles <- c(nexFiles, choppedFilename)
    }
    NTAXvector <- vector(mode = "integer")
    NCHARvector <- vector(mode = "integer", length = length(nexFiles))
    MATRIXlist <- list()
    newline <- ""
    NEXUStail <- c("  ;", "END;")
    # iterate over files to read relevant data
    for (i in seq_along(nexFiles)) {
        iNEXUS <- readLines(nexFiles[i])
        NEXUSheader <- iNEXUS[1:grep(pattern = "matrix", x = iNEXUS, ignore.case = TRUE)]
        lineChar <- grep(pattern = NCHARregex,
                         x = iNEXUS,
                         value = TRUE,
                         ignore.case = TRUE)
        charExp <- regexpr(NCHARregex, lineChar)
        iNCHAR <- regmatches(lineChar, charExp)
        iNCHAR <- gsub(pattern = "NCHAR[[:blank:]]*=[[:blank:]]*", replacement = "",
                       x = iNCHAR, ignore.case = TRUE)
        iNCHAR <- as.integer(iNCHAR)
        #cat("Number of characters", iNCHAR, class(iNCHAR), "\n", sep = " ")
        NCHARvector[i] <- iNCHAR
        #    iNTAX
        lineTax <- grep(pattern = NTAXregex,
                         x = iNEXUS,
                         value = TRUE,
                         ignore.case = TRUE)
        taxExp <- regexpr(NTAXregex, lineTax)
        iNTAX <- regmatches(lineTax, taxExp)
        iNTAX <- gsub(pattern = "NTAX[[:blank:]]*=[[:blank:]]*", replacement = "",
                      x = iNTAX, ignore.case = TRUE)
        iNTAX <- as.integer(iNTAX)
        #cat("Number of taxa", iNTAX, class(iNTAX), "\n\n", sep = " ")
        NTAXvector <- c(NTAXvector, iNTAX)
        # test that all taxa were the same across matrices
        #cat(length(table(NTAXvector)), " elements in NTAXvector\n",  sep = "")
        if (length(table(NTAXvector)) != 1) {
            stop("All nexus files must be of the same taxon count!\n")
        }
        #    iMATRIX
        iMATRIX <- iNEXUS[7:(length(iNEXUS) - 3)]
        MATRIXlist <- c(MATRIXlist, newline, iMATRIX)
    }
    
    # build concatenated components
    # NTAX  is the same regardless of the datatype nature
    NTAX <- paste("NTAX=", as.integer(names(table(NTAXvector))), sep = "")
    # NCHAR and DATATYPE vary for dna-only or dna+morpho matrices
    if (morpho) {
        NCHARdna <- sum(NCHARvector[-length(NCHARvector)])
        NCHARmorph <- sum(NCHARvector)
        NCHAR <- paste("NCHAR=", NCHARmorph, "", sep = "")
        DATATYPE <- paste("DATATYPE=MIXED(DNA:1-",
                          NCHARdna,
                          ",STANDARD:",
                          (NCHARdna + 1),
                          "-",
                          NCHARmorph,
                          ")", 
                          sep = "")
    } else {
        NCHAR <- paste("NCHAR=", sum(NCHARvector), sep = "")
        DATATYPE <- "DATATYPE=DNA"
    }
    # matrix is always the same
    # remove the newline at the beginning
    MATRIXlist <- MATRIXlist[-1]
    MATRIX <- unlist(MATRIXlist)
    
    # paste all vectors together
    outputNEXUS <- c(NEXUSheader,
                     MATRIX,
                     NEXUStail)
    
    # replace the appropriate parts
    # NCHAR
    outputNEXUS <- gsub(pattern = NCHARregex,
                        replacement = NCHAR,
                        x = outputNEXUS,
                        ignore.case = TRUE)
    outputNEXUS <- gsub(pattern = NTAXregex,
                        replacement = NTAX,
                        x = outputNEXUS,
                        ignore.case = TRUE)
    outputNEXUS <- gsub(pattern = DATATYPEregex,
                        replacement = DATATYPE,
                        x = outputNEXUS,
                        ignore.case = TRUE)
    outputNEXUS <- gsub(pattern = INTERLEAVEregex,
                        replacement = "INTERLEAVE=YES",
                        x = outputNEXUS,
                        ignore.case = TRUE)
    from_ape <- sum(grepl(pattern = "write.nexus.data.R",
                          x = outputNEXUS, ignore.case = TRUE))
    if (from_ape) {
        outputNEXUS[2] <- paste("[Data written by tbea::concatNexus ",  Sys.time(), "]", sep = "")
    }
    # write the summary table with the dimmension of each partition to be included into the MrBayes script
    endElem <- vector(mode = "integer", length = length(NCHARvector))
    for (i in seq_along(NCHARvector)) {
        endElem[i] <- sum(NCHARvector[1:i])
    }
    startElem <- vector(mode = "integer", length = length(NCHARvector))
    startElem[1] <- 1
    for (i in 1:(length(NCHARvector) - 1)) {
        startElem[i+1] <- endElem[i] + 1
    }
    summaryTable <- data.frame(file = nexFiles, start = startElem, end = endElem)
    writeLines(text = outputNEXUS, con = filename)
    write.table(x = summaryTable, file = sumFilename, sep = "\t", row.names = FALSE)
}

