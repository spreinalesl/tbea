#' crossplot: Plot the median and HPD interval bars for pairs of distribution 
#' 
#' @param log1Path character vector of length 1. Path to the first log file.
#'
#' @param log2Path character vector of length 1. Path to the second log file.
#'
#' @param skip.char character vector of length 1, with '#' as default value.
#' Which symbol is used as a comment. This will allow to ignore lines which
#' start with the symbol when reading data.
#' 
#' @param pattern character vector of length 1. the pattern for subsetting the
#' columns containing the data to be plotted.
#'
#' @param idx.cols either an integer vector with the position of the columns to
#' pick, or a character vector with the column names to pick. Defaults to NULL.
#' 
#' @param bar.lty  The line type to be used as error bars.
#'
#' @param bar.lwd As above but the width
#' 
#' @param identity.lty The line type to be used in
#' the identity y = x line
#'
#' @param identity.lwd As above but the width.
#'
#' @param extra.space numeric vector of length 1. How much space to be allowed
#' in both xlim and ylim depending on the smallest value in highest density
#' intervals plus or minus extra.space. A value of 0.5 units on the dimension
#' of interest is used by default.
#'
#' @param ... Optional arguments to be passed to `plot` such as `main`, `xlab`,
#' `ylab`, `pch` and `cex`.
#'
#' @return This function returns nothing, it plots to the graphical device.
#'
#' @author Gustavo A. Ballen
#'
#' @details The function produces a crossplot, which is a scatterplot where we
#' are comparing two distributions associated to each point by means of the
#' medians as the points, and the highest density intervals as bars around the
#' point. For instance, x may represent the prior of a set of parameters while
#' y represents the posterior. Error bars on the x axis then are highest density
#' intervals from the prior, and those on the y axis represent the interval for
#' the posterior.
#'
#' This function can also be used to compare two independent runs for
#' (visual) convergence: If they are sampling the same posterior distribution,
#' then they should fall on the identity y=x line.
#' 
#' @examples
#' \dontrun{
#' crossplot(log1Path="log1.tsv", log2Path="log2.tsv", skip.char="#",
#'           pattern="par", cols=NULL, bar.lty=1, bar.lwd=1,
#'           identity.lty=2, identity.lwd=1,
#'           extra.space=0.5, main="My plot", xlab="log 1 (prior)", ylab="log 2 (posterior)", pch=19)
#' }
#' @export
#' @importFrom coda mcmc HPDinterval
#' @importFrom graphics plot segments abline points
#' @importFrom stats median
#' @importFrom utils read.delim
#' 
crossplot <- function(log1Path, log2Path, skip.char="#", pattern=NULL,
                      idx.cols=NULL, bar.lty, bar.lwd, identity.lty, identity.lwd,
                      extra.space=0.5, ...) {
    # made sure that pattern and cols will not interact when subsetting
    if (is.null(pattern) & is.null(idx.cols)) {
        stop("Either a pattern to look up at column names or the column
              positions are required\n")
    }
    if (!is.null(pattern) & !is.null(idx.cols)) {
        stop("Only one of the arguments is allowed at a time: Either
              `pattern` or `idx.cols` must be non-null\n")
    }
    # read if using a text pattern
    if (!is.null(pattern)) {
        # read the log files
        datLog1 <- read.delim(file = log1Path, header = TRUE,
                               comment.char = skip.char,
                               stringsAsFactors = FALSE)
        datLog1 = datLog1[, grep(pattern, colnames(datLog1))]
            
        datLog2 <- read.delim(file = log2Path, header = TRUE,
                               comment.char = skip.char,
                               stringsAsFactors = FALSE)
        datLog2 = datLog2[, grep(pattern, colnames(datLog2))]
    }
    # read if using column indices
    if (!is.null(idx.cols)) {
        datLog1 <- read.delim(file = log1Path, header = TRUE,
                               comment.char = skip.char,
                               stringsAsFactors = FALSE)
        datLog1 = datLog1[, idx.cols]
            
        datLog2 <- read.delim(file = log2Path, header = TRUE,
                               comment.char = skip.char,
                               stringsAsFactors = FALSE)
        datLog2 = datLog2[, idx.cols]
    }
    ## Calculate a summary table for all the log files
    
    # for the first log
    log1Stats <- data.frame(stringsAsFactors = FALSE)
    for (j in seq_along(datLog1)) {
        var <- datLog1[j]
        varName <- colnames(var)
        medianj <- median(var[, , drop = TRUE], na.rm = TRUE)
        HPDj <- HPDinterval(mcmc(var))
        HPDlowerj <- HPDj[, "lower"]
        HPDupperj <- HPDj[, "upper"]
        iterStats <- data.frame(varName, medianj, HPDlowerj, HPDupperj,
                                stringsAsFactors = FALSE)
        log1Stats <- rbind(log1Stats, iterStats)
    }
    # for the second log
    log2Stats <- data.frame(stringsAsFactors = FALSE)
    for (j in seq_along(datLog2)) {
        var <- datLog2[j]
        varName <- colnames(var)
        medianj <- median(var[, , drop = TRUE], na.rm = TRUE)
        HPDj <- HPDinterval(mcmc(var))
        HPDlowerj <- HPDj[, "lower"]
        HPDupperj <- HPDj[, "upper"]
        iterStats <- data.frame(varName, medianj, HPDlowerj, HPDupperj,
                                stringsAsFactors = FALSE)
        log2Stats <- rbind(log2Stats, iterStats)
    }
    
    ### then actually plot the thing. 
    plot(log1Stats$medianj,
         log2Stats$medianj,
         xlim = c((min(log1Stats$HPDlowerj) - extra.space),
         (max(log1Stats$HPDupperj) + extra.space)),
         ylim = c((min(log2Stats$HPDlowerj) - extra.space),
         (max(log2Stats$HPDupperj) + extra.space)), ...)
    
    for (j in 1:nrow(log1Stats)) {
        segments(x0 = log1Stats[j, "medianj"],
                 x1 = log1Stats[j, "medianj"],
                 y0 = log2Stats[j, "HPDlowerj"],
                 y1 = log2Stats[j, "HPDupperj"],
                 lty=bar.lty,
                 lwd=bar.lwd)
    }
    for (j in 1:nrow(log2Stats)) {
        segments(y0 = log2Stats[j, "medianj"],
                 y1 = log2Stats[j, "medianj"],
                 x0 = log1Stats[j, "HPDlowerj"],
                 x1 = log1Stats[j, "HPDupperj"],
                 lty=bar.lty,
                 lwd=bar.lwd)
        
    }
    # Add the y = x line
    abline(a = 0, b = 1, lwd=identity.lwd, lty=identity.lty)
    # overlay the points to the lines in the plot
    points(x=log1Stats$medianj,
           y=log2Stats$medianj, ...)    
}
