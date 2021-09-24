#' Constructing a curve for the user-specified lognormal prior using Beast2 parameters
#'
#' @param M Mean of the lognormal density in Beast2.
#' 
#' @param S Standard deviation of the lognormal density in Beast2.
#' 
#' @param meanInRealSpace Whether to plot the mean on the real- or log-space (i.e., apply log(M) before plotting). Please see under details.
#' 
#' @param offset Hard lower bound.
#' 
#' @param from,to,by Starting and ending point to calculate considering the offset as zero. That is, from will affect produce a starting point of (offset + from) and an ending point of (offset + to). By sets the step size of the sequence from `from` to `to` each `by` steps.
#'
#' @return A matrix of two columns consisting of the x and y values of the lognormal density.
#'
#' @details This function creates a matrix of x,y values given parameters of a lognormal density as specified in the program Beast2. It's main purpose is for plotting but other uses such as similarity quantification are available. Please note that the value of mean depends on whether we expect it to be in real or log space. Please refer to Heath (2015) for more info: \href{http://phyloworks.org/workshops/DivTime_BEAST2_tutorial_FBD.pdf}{Heath, T. A. (2015). Divergence Time Estimation using BEAST v2}.
#'
#' @examples
#'
#' # Generate a matrix for the lognormal density with mean 1 and standard deviation 1, with mean
#' # in real space, and spanning values in x from 0 to 10
#' lognormalBeast(M = 1, S = 1, meanInRealSpace = TRUE, from = 0, to = 10)
#' # The same as above but with an offset of 10, that is, the curve starts at 10 as if it was 0
#' # to values will start in (offset + from) and finish in (offset + to)
#' lognormalBeast(M = 1, S = 1, meanInRealSpace = TRUE, offset = 10, from = 0, to = 10)
#' @export
#' @importFrom stats dlnorm

lognormalBeast <- function(M, S, meanInRealSpace = TRUE, offset = 0, from = NULL, to = NULL, by = 0.05) {
    
    # some input checks
    if (is.null(from) | is.null(to)) {
        stop("Both arguments \'from\' and \'to\' must be defined")
    }
    
    # check that S is proper
    if (S <= 0) {
        stop("The standard deviation \'S\' must be a positive number higher than zero")
    }
    
    # make sure that we can apply a logarithm on the M defined by the user
    if (meanInRealSpace == TRUE) {
        if (M <= 0) {
            stop("The mean \'M\' must be a positive number higher than zero")
        }
        M <- log(M)
    }
    x <- seq(from = from, to = to, by = by) + offset
    y <- dlnorm(seq(from = from, to = to, by = by), M, S)
    LnDataPoints <- cbind(x = x, y = y)
    return(LnDataPoints)
}
