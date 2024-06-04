#' table2nexus: Read a data matrix in delimited format and convert
#' into a data matrix in nexus format
#' 
#' @param path a character vector of length 1 with the path to the table
#' file.
#'
#' @param datatype a character vector of length 1 with the desired datatype.
#' Possible values are STANDARD, DNA, RNA, or PROTEIN.
#' Multicharacter types such as continuous or nucleotide are not supported.
#'
#' @param header a logical vector of length 1 indicating whether the table file
#' has a header. Defaults to FALSE.
#'
#' @param sep a character vector of length 1 telling the kind of separator in
#' the table file. Defaults to comma ",".
#'
#' @param con the connection to which the matrix should be returned. Defaults
#' to stdout(), that is, return the text to the console. If writing to a file,
#' then this should be the path to the output file.
#' 
#' @return This function writes to the connected required a matrix in nexus
#' format for a morphological dataset (that is, datatype=standard).
#'
#' @author Gustavo A. Ballen
#'
#' @details This function will concatenate matrices in nexus format (mandatory) and write to the disk the output and summary information on the partitions. It requires that the input matrices all share the same taxa in the same positions.
#' 
#' @examples
#' \dontrun{
#' # this will return the matrix to the console rather than to a file
#' table2nexus(path="morpho.csv", datatype="standard", header=FALSE, sep=",")
#' }
#' @export
#' @importFrom utils read.delim

table2nexus <- function(path, datatype = c("standard", "dna", "rna", "protein"), header = FALSE, sep=",", con=stdout()) {
    DATATYPE <- toupper(match.arg(datatype))
    df <- read.delim(path, sep=sep, header=header)
    if (header) {
        colnames(df) <- NULL
    }
    taxa <- df[[1]]
    NTAX <- length(taxa)
    df <- df[, -1]
    NCHAR <- ncol(df)
    chars <- apply(X=df, MARGIN=1, FUN=function(x) paste(x, collapse=""))
    NEXUSheader <- c("#NEXUS",
                     "[Data written by table2nexus, _TIMESTAMP]",
                     "BEGIN DATA;",
                     "  DIMENSIONS NTAX=_NTAX NCHAR=_NCHAR",
                     "  FORMAT DATATYPE=_DATATYPE MISSING=? GAP=- INTERLEAVE=NO;",
                     "  MATRIX")
    NEXUSheader <- gsub(pattern="_TIMESTAMP", replacement=Sys.time(), x=NEXUSheader)
    NEXUSheader <- gsub(pattern="_DATATYPE", replacement=DATATYPE, x=NEXUSheader)
    NEXUSheader <- gsub(pattern="_NTAX", replacement=NTAX, x=NEXUSheader)
    NEXUSheader <- gsub(pattern="_NCHAR", replacement=NCHAR, x=NEXUSheader)
    DATA <- paste("    ", taxa, "      ", chars, sep="")
    OUTPUT <- c(NEXUSheader, DATA, "  ;", "END;")
    writeLines(OUTPUT, con=con)
}
    
