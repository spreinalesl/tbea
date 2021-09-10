#' Reduced chi-square test or mean square weighted deviation (mswd) test
#' 
#' @param age A vector of age radiometric age estimates
#'
#' @param sd A vector of the standard deviation corresponding to each element in \code{age}
#' 
#' @return A numeric vector of length one with the p-value corresponding to the test.
#'
#' @details From Ludwig (2003:646): "By convention,  probabilities  of  fit  greater  than  0.05  are  generally
#' considered  as  arguably  satisfying  the  mathematical  assumptions  of  an  isochron,  while  lower
#' probabilities are generally taken as indicating the presence of “geological” scatter, and hence a significant
#' possibility of bias in the isochron age.".
#' The null hypothesis is that the isochron conditions hold.
#' 
#' @examples
#' data(laventa)
#' # Do the age estimates for the boundaries of the Honda Group (i.e., samples at meters 56.4
#' # and 675.0) conform to the isochron hypothesis?
#' hondaIndex <- which(laventa$elevation == 56.4 | laventa$elevation == 675.0) 
#' mswd.test(age = laventa$age[hondaIndex], sd = laventa$one_sigma[hondaIndex])
#' # The p-value is smaller than the nominal alpha of 0.05, so we can reject the null
#' # hypothesis of isochron conditions
#'
#' # Do the age estimates for the samples JG-R 88-2 and JG-R 89-2 conform to the isochron hypothesis?
#' twoLevelsIndex <- which(laventa$sample == "JG-R 89-2" | laventa$sample == "JG-R 88-2")
#' dataset <- laventa[twoLevelsIndex, ]
#' # Remove the values 21 and 23 because of their abnormally large standard deviations
#' mswd.test(age = dataset$age[c(-21, -23)], sd = dataset$one_sigma[c(-21, -23)])
#' # The p-value is larger than the nominal alpha of 0.05, so we can
#' # not reject the null hypothesis of isochron conditions
#' @export
#' @importFrom stats pchisq

mswd.test <- function(age, sd) {
    # check that input is a vector of type numeric
    if (!is.numeric(age)) {
        stop("age is not a numeric vector!")
    }
    if (!is.numeric(sd)) {
        stop("sd is not a numeric vector!")
    }
    
    # calculate the weighted mean
    weightedMean <- sum(age/sd^2)/sum(1/sd^2)
    
    # Mean squared weighted deviation (a.k.a. reduced chi-square statistic)
    mswd <- 1/(length(age) - 2)*sum(((age - weightedMean)^2)/sd^2)

    # p-value    
    pvalue <- pchisq(mswd*(length(age) - 2), (length(age) - 2), lower.tail = FALSE)
    
    return(pvalue)
}
