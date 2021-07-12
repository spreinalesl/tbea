#' fasta2nexus: Function for converting molecular alignments from fasta to nexus format
#' 
#' @param path A vector of type 'character' with the path to the fasta alignments.
#'
#' @param outpath A vector of type 'character' with the path to the nexus matrices. Defaults to NULL, so that the output files are written into the same directory declared in \code{path} 
#'
#' @param pattern A vector of type 'character' with the string (also supports regular expressions) to be used as keyword for selecting the fasta files. The most basic case is to use ".fasta$" for a file ending with the extension ".fasta".
#'
#' @param wholeAlign Whether to fuse the fasta alignments into a concatenated molecular-only, continuous nexus matrix. Defaults to TRUE.
#' 
#' @return This function writes to the disk several files, at least one nexus originally from a fasta file, and potentially a concatenated file if several fasta are provided.
#'
#' @author Gustavo A. Ballen
#'
#' @details This function will convert from fasta to nexus, and optionally concatenate a single nexus with the content of all fasta files.
#' 
#' @examples
#' # Convert all fasta alignments into nexus matrices in a given path,
#' # with the output files in the same directory, for files
#' # ending with the pattern 'trimmed.nex'.
#' \dontrun{
#' fasta2nexus(path = "sequences", outpath = NULL, pattern = "trimmed.fasta$", wholeAlign = TRUE)
#' }
#' @export

fasta2nexus <- function(path, outpath = NULL, pattern, wholeAlign = TRUE) {    
    fastaFiles <- dir(path = path, pattern = pattern)
    if (!is.null(outpath)){
        dir.create(outpath)
    } else {
        outpath <- path
    }
    # read all fastaFiles datasets
    markers <- apex::read.multiFASTA(paste(path, fastaFiles, sep = "/"))
    markers
    if (wholeAlign) {
        wholeAlignNexus <- apex::concatenate(markers)
        ape::write.nexus.data(x = wholeAlignNexus, file = paste(outpath, "wholeAlign.nex", sep = "/"), interleaved = FALSE)
    }
    for (i in seq_along(markers@dna)) {
        cat("Writing nexus dataset ", names(markers@dna[i]), "\n", sep = "")
        ape::write.nexus.data(x = markers@dna[[i]],
                              file = paste(outpath, "/", names(markers@dna[i]), ".nex", sep = ""),
                              interleaved = FALSE)
    }
}
