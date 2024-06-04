#' Calculate the Intersection Between Two Densities
#'
#' @param d1,d2 Either two vectors of empirical (i.e., MCMC-produced) values OR a \code{data.frame}/\code{matrix} with columns x and y for values fitted to a density from which to calculate areas. If \code{rawData} is set to \code{TRUE} in any instance, the data must be placed in vectors and not multidimensional objects.
#'
#' @param splits A numerical argument controling the number of subdivisions of the intersection area for numerical integration
#'
#' @param rawData Are d1 and/or d2 raw data for which a density should be calculated? A vector of length two containing logical values indicating whenther any of the arguments d1 or d2 are raw data or whether the user is inputing already calculated densities (e.g., the output from the density, curve, or dDIST functions, or any two-dimension object with x and y values)
#'
#' @param plot Should a plot be produced?
#'
#' @param colors A vector of three colors, namely, color of the \code{d1} density (e.g., the prior), color of the \code{d2} density e.g., the posterior), and color of the intersection.
#'
#' @param x_limit Whether to define the xlim form the min-max of the combined density x-values
#'
#' @param ... Further arguments to pass to the graphical functions such as \code{lines} and \code{plot} internally (e.g., \code{main}, \code{xlim}, \code{ylim}, \code{xlab}, \code{ylab}, etc.).
#'
#' @return A numeric vector with the value of the intersection between two densities. As a side effect, a plot is produced to an active (or new) graphical device.
#'
#' @details Similarity is measured as the overlapping portion between two densities. It has a value between 0 and 1. The values of the vector rawData determine the behavior of the function and therefore attention must be paid to their consistence with the nature of arguments d1 and d2. Despite the function was designed in order to allow to quantify similarity between the posterior and the prior, this can be used to quantify any overlap between two given densities and for any other purpose.
#' @examples
#' \donttest{
#' # Set seed and colors to use in plots in the order: Prior, posterior, and intersection
#' set.seed(1985)
#' colors <- c("red", "blue", "lightgray")
#' # Similarity between two identical distributions
#' below <- measureSimil(d1 = rnorm(1000000, mean = 0, 1),
#'                        d2 = rnorm(1000000, mean = 0, 1),
#'                        main = "Comp. similarity",
#'                        colors = colors)
#' legend(x = "topright", legend = round(below, digits = 2))
#' # Similarity in two distributions partially overlapping
#' below <- measureSimil(d1 = rnorm(1000000, mean = 3, 1),
#'                        d2 = rnorm(1000000, mean = 0, 1),
#'                        main = "Partial similarity",
#'                        colors = colors)
#' legend(x = "topright", legend = round(below, digits = 2))
#' # Similarity in two completely-different distributions
#' below <- measureSimil(d1 = rnorm(1000000, mean = 8, 1),
#'                        d2 = rnorm(1000000, mean = 0, 1),
#'                        main = "Comp. dissimilarity",
#'                        colors = colors)
#' legend(x = "topright", legend = round(below, digits = 2))
#' # Don't plot, just return the intersection
#' measureSimil(d1 = rnorm(1000000, mean = 3, 1),
#'               d2 = rnorm(1000000, mean = 0, 1),
#'               plot = FALSE)
#' }
#' @export
#' @importFrom graphics lines polygon
#' @importFrom stats approxfun density


measureSimil <- function(d1, d2, splits = 500, rawData = c(TRUE, TRUE), plot = TRUE, x_limit = "auto", colors = c("red", "blue", "gray"), ...) {
    # carry out density calculation for raw data argument d1
    if (rawData[1] == TRUE) {
        d1 <- as.data.frame(density(d1)[c("x", "y")])
    }
    # carry out density calculation for raw data argument d2
    if (rawData[2] == TRUE) {
        d2 <- as.data.frame(density(d2)[c("x", "y")])
    }
    # interpolation functions
    f1 <- approxfun(d1$x, d1$y)
    f2 <- approxfun(d2$x, d2$y)
    # interval where densities overlap
    overlap <- c(max(min(d1$x), min(d2$x)), min(max(d1$x), max(d2$x)))
    if (overlap[1] > overlap[2]) {
        areaBelow <- 0
    }
    # sequence along the overlap vector for a total length of splits
    i <- seq(min(overlap), max(overlap), length.out = splits) 
    # calculate the minimum value between both curves that corresponds to the intersection of densities, or in set notation, {d1 & d2}
    below <- sapply(X = i, FUN = function(x) {min(c(f1(x), f2(x)))})
    # calculate the area below the intersection of densities by numerical integration using rectangles of heigh = below, and base = overlap/splits. The larger the number of splits, the more accurate the estimation 
    areaBelow <- sum(below)*((overlap[2] - overlap[1])/splits)
    # If there is no overlap at all, the areaBelow calculation is improper and therefore an NA is returned. This must be corrected so that zero is assigned to cases where the first element in the overlap vector is larger-than or equal to the second element 
    if (overlap[1] >= overlap[2]) {
        areaBelow <- 0
    }
    # Plot both densities and the intersection
    # plot if needed
    if (plot == TRUE) {
        # whether to define the xlim form the min-max of the combined density x-values
        if (x_limit[1] == "auto") {
            xlim <- c(min(c(d1$x, d2$x)),
                     max(c(d1$x, d2$x)))
        } else {
            xlim = x_limit
        }        
        plot(d1,
             type = "l",
             xlim = xlim, 
             col = colors[1], ...)
        lines(d2, col = colors[2], ...)
        polygon(i, below, col = colors[3])
    }
    # return the value of the intersection area 
    return(areaBelow)
}
