#' Function for estimation of probability density function parameters through quadratic optimization
#' 
#' @param p A numeric vector of percentiles.
#'
#' @param q A numeric vector of observed quantiles, might come from a HPD from a previous study (along with a median), or from other sources of prior information. See Details.
#'
#' @param pdfunction A character vector (of length one) with the name of the PDF function of interest. Technically this argument supports any PDF function of the form pDIST (e.g., \code{pnorm}, \code{ppois}, \code{pexp}).
#' 
#' @param params A character vector with the name of the parameter(s) to optimize in the probability density function. These should match the parameter names of the respective PDF function, e.g., \code{"lambda"} in the function \code{ppois}
#'
#' @param initVals A numeric vector with default value \code{NULL}. It allows the user to provide initial values, althought this is discouraged in most cases.
#' 
#' @param output One of two possible values: \code{"complete"} and \code{"parameters"}. For the latter the complete output of the \code{optim} function is returned with information on convergence and squared errors (that might be useless for simple cases) or just the parameters.
#'
#' @return Either a list with the complete output of convergence, squared errors and parameter values, or just a vector of parameter values. Depends on the value of \code{output}.
#' Warnings may be triggered by the function \code{optim} since the optimization is a heuristic process, whenever a given iteration results in an invalid value for a given combination of parameters, the \code{optim} function tries another combination of values but inform the user about the problem through a warning. In general these can be safely disregarded.
#'
#' @author Main code by Gustavo A. Ballen with important contributions in expression call structure and vectorized design by Klaus Schliep (\email{Klaus.Schliep@umb.edu}).
#'
#' @details This function comes handy whenever we have some values of uncertainty, (e.g., confidence intervals, HPDs, biostratigraphic age constrains) and want to express it in the form of a probability density function of the form \eqn{P(x;\theta)}. As we have some values (the quantiles) already and their corresponding percentiles, all we need is a way to approximate the parameters \eqn{\theta} that produce the same combination of quantiles for the given percentiles under a given PDF. This is carried out through optimization of a quadratic error function. This is accomplished through the function \code{optim}. For instance, if the estimated age of a fossil is Lutetian, in the Eocene (41.2 to 47.8 Ma), and we want to model such uncertainty through a normal distribution, we could assume that these age boundaries are the quantiles for percentiles 0.025 and 0.975 respectively, and add a thir pair with the midpoint corresponding to the percentile 0.5. This is all the information needed in order to estimate the parameters \code{mean} and \code{sd} in the functiono \code{pnorm}. 
#' 
#' @examples
#' # Find the best parameters for a standard normal density that fit the observed quantiles
#' # -1.644854, 0, and 1.644854, providing full output for the calculations in the form of
#' # a list
#' findParams(q = c(-1.959964, 0.000000, 1.959964),
#'            p = c(0.025, 0.50, 0.975),
#'            output = "complete",
#'            pdfunction = "pnorm",
#'            params = c("mean", "sd"))
#'
#' # Given that we have prior on the age of a fossil to be 1 - 10 Ma and that we want to
#' # model it with a lognormal distribution, fin the parameters of the PDF that best reflect
#' # the uncertainty in question (i.e., the parameters  for which the observed quantiles are
#' # 1, 5.5, and 10, assuming that we want the midpoint to reflect the mean of the PDF.
#' findParams(q = c(1, 5.5, 10),
#'            p = c(0.025,  0.50, 0.975),
#'            output = "complete",
#'            pdfunction = "plnorm",
#'            params = c("meanlog", "sdlog"))
#' @export
#' @importFrom stats optim

findParams <- function(q, p, output = "complete", pdfunction, params, initVals = NULL) {
    # check that p and q are of the same length because the quadratic function requires them paired
    if (length(p) != length(q)) {
        stop("The length of p and q must be equal, we need as many quantiles q as probabilities p.")
    }
    #  calculate init values if the user did not provide any 
    if (is.null(initVals)) {
        warning("initVals not provided, will use the mean of quantiles in q for _each_ of the parameters. Parameter estimates might not be reliable even though convergence is reported.")
        initVals <- rep(mean(q), times = length(params))
    }    
    # construct the call as a list
    lpars <- length(params)
    cl <- vector("list", 2  + lpars)
    cl[[1]] <- as.name(pdfunction)
    cl[[2]] <- q
    names(cl) <- c(NA, "q", params)
    mode(cl) <- "call"
    # the quadratic function will minimize the error around estimates
    quadraticFun <- function(x) {
        cl[3:(lpars+2)] <- x
        res <- eval(cl)
        sum((res - p)^2)
    }
    # pick the method for optimization based on the number of parameters
    if (lpars == 1) {
        res <- optim(initVals, quadraticFun, method = "BFGS")
    } else {
        res <- optim(initVals, quadraticFun)
    }
    # return depending on the desired output    
    if (output == "parameters") {
        return(res$par)
    } else {
        return(res)
    }
}
