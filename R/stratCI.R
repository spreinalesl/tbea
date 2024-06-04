#' stratCI: Estimate the confidence intervals of endpoints in
#' stratigraphic intervals
#' 
#' @param times a vector of occurrences in time for which we want to
#' estimate the endpoints of the stratigraphic interval
#'
#' @param method a character describing which method to use, either
#' 'Strauss-Sadler89' or 'Marshall94'.
#'
#' @param nparams A character indicating whether to estimate one or two
#' parameters, possible values are 'one.par' and 'two.par'.
#'
#' @param C numeric indicating the confidence level, e.g. 0.95
#'
#' @param endpoint used only for nparams = 'one.par'. Possible values
#' are 'first' and 'last'.
#'
#' @param confidence the confidence interval level, usually 0.95.
#'
#' @param quantile the desired confidence level for a quantile representing
#' the brackets around the confidence interval.
#'
#' @return A named vector when using the 'Strauss-Sadler89' method, or
#' an unnamed vector when using the 'Marshall94' method.
#'
#' @details For method='Strauss-Sadler89' we need to provide
#' `nparams`, `C`, and `endpoint`. For method='Marshall94' we need to
#' provide `confidence` and `quantile`.
#'
#' @author Gustavo A. Ballen.
#'
#' @examples
#' data(andes)
#' andes <- andes$ages
#' # remove missing data
#' andes <- andes[complete.cases(andes)]
#' # remove outliers
#' andes <- sort(andes[which(andes < 10)])
#' stratCI(andes, method="Strauss-Sadler89",
#'         nparams="one.par", C=0.95, endpoint="first")
#' stratCI(andes, method="Strauss-Sadler89",
#'         nparams="one.par", C=0.95, endpoint="last")
#' stratCI(andes, method="Strauss-Sadler89",
#'         nparams="two.par", C=0.95)
#' stratCI(andes, method="Marshall94", confidence = 0.95,
#'         quantile = 0.8)
#' stratCI(andes, method="Marshall94", confidence = 0.95,
#'         quantile = 0.95)
#' 
#' @export

stratCI <- function(times, method, nparams, C, endpoint, confidence, quantile) {
    if (method == "Strauss-Sadler89") {
        return(.straussSadler89(times, nparams, C, endpoint))
    }
    if (method == "Marshall94") {
        return(.marshall94(times, confidence, quantile))
    }
    stop("Method must be one of Strauss-Sadler89 or Marshall94")
}

########## Strauss and Sadler 89 method for continuous sampling

#' .straussSadler89: Estimate confidence intervals using the method
#' of Strauss-Sadler89
#'
#' @param times a vector of occurrences in time for which we want to
#' estimate the endpoints of the stratigraphic interval
#'
#' @param nparams A character indicating whether to estimate one or two
#' parameters, possible values are 'one.par' and 'two.par'.
#'
#' @param C numeric indicating the confidence level, e.g. 0.95
#'
#' @param endpoint used only for nparams = 'one.par'. Possible values
#' are 'first' and 'last'.
#'
#' @return a call to either .rC or .rC2p.
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user but rather via `strtCI`.
#' 
#' @noRd

.straussSadler89 <- function(times, nparams, C, endpoint) {
    H <- length(times)
    mint <- min(times)
    maxt <- max(times)
    R <- maxt - mint
    x <- list(H = H, min = mint, max = maxt, R = R)
    if (nparams == "one.par") {
        return(.rC(x, C, endpoint))
    }
    if (nparams == "two.par") {
        return(.rC2p(x, C))
    } else {
        stop("nparams must be one- or two-parameter cases of Strauss & Sadler '89")
    }
}

#' .rC: One-parameter case of Strauss-Sadler89
#' 
#' @param x A vector of type numeric with time data points.
#'
#' @param C numeric indicating the confidence level, e.g. 0.95
#'
#' @param endpoint used only for nparams = 'one.par'. Possible values
#' are 'first' and 'last'.
#'
#' @return A named vector with the calculated values for the one-parameter
#' case
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user but rather via `.straussSadler89`. One-parameter case for the
#' estimator of early age or min age. alpha comes from the Eq. 20 and
#' formation of the confidence interval from Eq. 12
#' 
#' @noRd

.rC <- function(x, C, endpoint) {
    a <- x$R*((1 - C)^(-1/(x$H - 1)) - 1)
    if (endpoint == "first") {
        c(minObs = x$min, maxObs = x$max, maxEst = x$max + a, H = as.integer(x$H), alphaR = a, alpha = a/x$R)
    } else {
        c(minEst = x$min - a, minObs = x$min, maxObs = x$max, H = as.integer(x$H), alphaR = a, alpha = a/x$R)
    }
}

#' .rC2p: Two-parameter case of Strauss-Sadler89 
#' 
#' @param x A vector of type numeric with time data points.
#'
#' @param C numeric indicating the confidence level, e.g. 0.95
#'
#' @return A named vector with the calculated values for the one-parameter
#' case
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user. Two-parameter case for the estimator of early age or min age.
#' alpha comes from the Eq. 19 and formation of the confidence interval
#' from Eqs. 12 and 13 coupled. It calls the .alpha() function that
#' iteratively calculates alpha from Eq. 19.
#' 
#' @noRd

.rC2p <- function(x, C) {
    a <- .alpha(x$H, C)
    c(minEst = x$min - a*x$R, minObs = x$min, maxObs = x$max, maxEst = x$max + a*x$R, H = as.integer(x$H), alphaR = a*x$R, alpha = a)
}

#' .alpha: function for iteratively solving Eq. 19 in Strauss and Sadler '89.
# arguments as follows
#' 
#' @param H The number of data points.
#'
#' @param C numeric indicating the confidence level, e.g. 0.95
#'
#' @return Returns the calculated alpha for the two-parameter case.
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user.
#' 
#' @noRd

.alpha <- function(H, C) {
    alphae <- seq(from = 0, to = 30, by = 0.001)
    y <- 1 - 2*(1 + alphae)^(1 - H) + (1 + 2*alphae)^(1 - H)
    output <- alphae[which(abs(y - C) < 0.001)]
    mean(output)
}

####### Distribution-free non-random CIs (Marshall 1994) ##########

#' .marshall94:  
#' 
#' @param times a vector of occurrences in time for which we want to
#' estimate the endpoints of the stratigraphic interval
#'
#' @param confidence the confidence interval level, usually 0.95.
#'
#' @param quantile the desired confidence level for a quantile representing
#' the brackets around the confidence interval.
#'
#' @return An unnamed vector of length two with the lower and upper bounds
#' on the confidence interval.
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user but rather via `stratCI`.
#' 
#' @noRd

.marshall94 <- function(times, confidence, quantile) {
    # sorting the times is necessary for calculating correctly the gap sizes
    times <- sort(times)
    gapsizes <- sapply(2:length(times), FUN=function(x) times[x] - times[x-1])
    # sorting gap sizes is necessary for working with quantiles
    gapsizes <- sort(gapsizes)
    #print(gapsizes)
    lower <- .lowerBound(confidence, length(gapsizes), quantile)
    upper <- .upperBound(confidence, length(gapsizes), quantile)
    output <- c(max(times)+gapsizes[lower], max(times)+gapsizes[upper])
    return(output)
}

#' .lowerBound:  
#' 
#' @param confidence the confidence interval level, usually 0.95.
#'
#' @param N number of gap sizes
#' 
#' @param quantile the desired confidence level for a quantile representing
#' the brackets around the confidence interval.
#'
#' @return The lower bound on the confidence interval.
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user.
#' 
#' @noRd

.lowerBound <- function(confidence, N, quantile) {
    gamma <- (1 - confidence)/2
    leftside <- NULL
    for(i in 0:N) {
        x <- i
        leftside <- c(leftside, (choose(N, x) * quantile^x * (1 - quantile)^(N - x))) # for x <= N
    }
    sums <- NULL
    for(i in seq_along(leftside)) {
        sums <- c(sums, sum(leftside[1:i]))
    }
    tests <- sums < gamma
    output <- sum(tests)
    if(output >= 1) {
        return(output)
    } else {
        message("Impossible to calculate lower bound\n")
    }
}

#' .upperBound:  
#' 
#' @param confidence the confidence interval level, usually 0.95.
#'
#' @param N number of gap sizes
#' 
#' @param quantile the desired confidence level for a quantile representing
#' the brackets around the confidence interval.
#'
#' @return The upper bound on the confidence interval. A warning and NA are
#' returned if the upper bound cannot be calculated for the desired
#' confidence and quantile.
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user.
#' 
#' @noRd

.upperBound <- function(confidence, N, quantile) {
    gamma <- (1 - confidence)/2
    rightside <- NULL
    for(i in 0:N) {
        x <- i
        rightside <- c(rightside, (choose(N, x) * quantile^x * (1 - quantile)^(N - x))) # for x <= N
    }
    sums <- NULL
    for(i in seq_along(rightside)) {
        sums <- c(sums, sum(rightside[1:i]))
    }
    tests <- sums > (1 - gamma)
    index <- 1:length(tests)
    output <- index[tests == TRUE][1]
    if(is.na(output) | (output >= N)) {
        warning("Impossible to calculate upper bound")
    } else {
        return(output)
    }
}
