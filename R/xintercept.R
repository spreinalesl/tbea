#' xintercept: Estimate the x-intercept of an empirical cdf 
#' 
#' @param x A vector of type numeric with time data points.
#'
#' @param method Either "Draper-Smith" or "Bootstrap". The
#' function will fail otherwise.
#'
#' @param alpha A vector of length one and type numeric with
#' the nominal alpha value for the Draper-Smith method, defaults
#' to 0.05.
#' 
#' @param p A vector of length two and type numeric with the
#' two-tail probability values for the CI. Defaults to 0.025
#' and 0.975.
#'
#' @param R The number of iterations to be used in the Bootstrap
#' method.
#' 
#' @param robust Logical value indicating whether to use robust
#' regression using `Rfit::rfit` (`robust = TRUE`) or ordinary
#' least squares `lm` (`robust = FALSE`).
#'
#' @return A named list with three elements: `param`, the value
#' of x_hat; `ci`, the lower and upper values of the confidence
#' interval on x; `ecdfxy`, the x and y points for the empirical
#' cumulative density curve.
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function will take a vector of time points, calculate
#' the empirical cumulative density function, and regress its values
#' in order to infer the x-intercept and its confidence interval. For
#' plotting purposes, it will also return the x-y empirical cumulative
#' density values.
#' 
#' @examples
#' data(andes)
#' ages <- andes$ages
#' ages <- ages[complete.cases(ages)] # remove NAs
#' ages <- ages[which(ages < 10)] # remove outliers
#' 
#' \donttest{
#' # Draper-Smith, OLS
#' draperSmithNormalX0 <- xintercept(x = ages, method = "Draper-Smith", alpha = 0.05, robust = FALSE)
#' # Draper-Smith, Robust fit
#' draperSmithRobustX0 <- xintercept(x = ages, method = "Draper-Smith", alpha = 0.05, robust = TRUE)
#' # Bootstrap, OLS
#' bootstrapNormalX0 <- xintercept(x = ages, method = "Bootstrap", p = c(0.025, 0.975), robust = FALSE)
#' # Bootstrap, Robust fit
#' bootstrapRobustX0 <- xintercept(x = ages, method = "Bootstrap", p = c(0.025, 0.975), robust = TRUE)
#' # plot the estimations
#' hist(ages, probability = TRUE, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3),
#'      xlim = c(0, 10), main = "CDF-based on confidence intervals", xlab = "Age (Ma)")
#' # plot the lines for the estimator of Draper and Smith using lm
#' arrows(x0 = draperSmithNormalX0$ci["upper"], y0 = 0.025, x1 = draperSmithNormalX0$ci["lower"],
#'        y1 = 0.025, code = 3, angle = 90, length = 0.1, lwd = 3, col = "darkblue")
#' # plot the lines for the estimator of Draper and Smith using rfit
#' arrows(x0 = draperSmithRobustX0$ci["upper"], y0 = 0.05, x1 = draperSmithRobustX0$ci["lower"],
#'        y1 = 0.05, code = 3, angle = 90, length = 0.1, lwd = 3, col = "darkgreen")
#' # plot the lines for the estimator based on bootstrap
#' arrows(x0 = bootstrapRobustX0$ci["upper"], y0 = 0.075, x1 = bootstrapRobustX0$ci["lower"],
#'        y1 = 0.075, code = 3, angle = 90, length = 0.1, lwd = 3, col = "darkred")
#' # plot a legend
#' legend(x = "topright", legend = c("Draper and Smith with lm", "Draper and Smith with rfit",
#'                                   "Bootstrap on x0"),
#'        col = c("darkblue", "darkgreen", "darkred"),  lty = 1, lwd = 3)
#' }
#' @export
#' @importFrom boot boot
#' @importFrom Rfit rfit
#' @importFrom stats coef cor ecdf lm qt quantile

xintercept <- function(x, method, alpha = 0.05, p = c(0.025, 0.975), R = 1000, robust){
    x <- sort(x)
    yfun <- ecdf(x)
    y <- yfun(x)
    ecdfx <- list(x=x, y=y)
    ecdfx$y <- 1 - ecdfx$y # in order to represent properly in time this information, we need the complement of cumulative probability
    if (method == "Draper-Smith") {
        if (!robust) {
            # classical regression
            regression <- lm(y ~ x, data = ecdfx)
        } else if (robust) {
            ## using robust regression methods
            regression <- Rfit::rfit(y ~ x, data = ecdfx)
        } else {
            stop("Robust must be one of TRUE or FALSE")
        }
        
        result <- .CI_DraperSmith_X0(x = regression, alpha = alpha)
    } else if(method == "Bootstrap") {
        result <- .CI_Boot_X0(x = ecdfx$x, y = ecdfx$y, p = p, R = R, robust = robust)
    } else {
        stop("Method must be either Draper-Smith or Bootstrap")
    }
    ecdfxy <- cbind(ecdfx$x, ecdfx$y)
    colnames(ecdfxy) <- c("x", "y")
    result <- list(param = result["x_hat"],
                   ci = result[c("lower", "upper")],
                   ecdf = ecdfxy)
    return(result)
}

#' .CI_DraperSmith_X0: Estimate the x-intercept with the method of Draper-Smith 
#' 
#' @param x A vector of type numeric with time data points.
#'
#' @param alpha A vector of length one and type numeric with the
#' nominal alpha value for the Draper-Smith method, defaults to 0.05.
#' 
#' @return A vector of length three with the values of lower, x_hat,
#' and upper in confidence interval and x-intercept
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by the
#' user but rather via `xintercept`.
#' 
#' @noRd
#' @importFrom boot boot
#' @importFrom Rfit rfit
#' @importFrom stats ecdf

.CI_DraperSmith_X0 <- function(x, alpha = 0.05) {
    if (inherits(x, "rfit")) {
        x$model <- data.frame(y = x$y,
                              x = x$x[,"x"],
                              stringsAsFactors = FALSE)
    }
    intercept <- coef(x)[1]              
    b1 <- coef(x)[2]                  
    Xbar <- mean(x$model[,2])           
    n <- length(x$model[,2])             
    tn_2 <- qt(alpha/2, n-2)            
    sxx <- sum(x$model[,2]^2) - sum(x$model[,2])^2 / n
    SSresidual <- (1-cor(x$model[,1], x$model[,2])^2) * 
                  (sum(x$model[,1]^2)-sum(x$model[,1])^2/n)
    S <- sqrt(SSresidual/(n-2))
    SEb1 <- S / sqrt(sxx)
    X0 <- - intercept / b1
    g <- (tn_2 / (b1/SEb1))^2
    left <- (X0 - Xbar) * g
    Right <- (tn_2 * S / b1) * sqrt( ((X0 - Xbar)^2/sxx) + (1 - g)/n)
    Xbounds <- X0 + (left + c(-1, 1)*Right) / (1 - g)

    out <- c(Xbounds[1], X0, Xbounds[2])
    names(out) <- c("lower", "x_hat", "upper")
    
    return(out)
}

#' .CI_Boot_X0: Estimate the x-intercept with the method of Bootstrap 
#' 
#' @param x,y Two vectors of type numeric with the ecdf coordinates.
#'
#' @param p A vector of length two and type numeric with the
#' two-tail probability values for the CI. Defaults to 0.025
#' and 0.975.
#'
#' @param R The number of iterations to be used in the bootstrap
#' method.
#' 
#' @param robust Whether to use robust regression using `Rfit::rfit`
#' or ordinary least squares `lm`.
#' 
#' @return A vector of length three with the values of lower, x_hat,
#' and upper in confidence interval and x-intercept
#'
#' @author Gustavo A. Ballen.
#'
#' @details This function is not intended to be called directly by
#' the user but rather via `xintercept`.
#' 
#' @noRd
#' @importFrom boot boot
#' @importFrom Rfit rfit
#' @importFrom stats ecdf

.CI_Boot_X0 <- function(x, y, p = c(0.025, 0.975), R = 1000, robust = FALSE) {
    d <- data.frame(x, y)
    bootObj <- boot::boot(d, 
                          function(d, i) {
                              if(robust) {
                                  fit <- Rfit::rfit(y ~ x, data = d[i,])
                              } else {
                                  fit <- lm(y ~ x, data = d[i,])
                              }
                        -coef(fit)[1]/coef(fit)[2] 
                    }, 
                    R = R)
    out <- c(quantile(bootObj$t, p)[1],
             bootObj$t0,
             quantile(bootObj$t, p)[2])
    names(out) <- c("lower", "x_hat", "upper")
    
    return(out)
}
