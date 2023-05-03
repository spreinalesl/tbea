#' Frequency of topologies in a tree sample
#' 
#' @param mphy An object of class multiPhylo
#' @param output A character indicating whether the tree indices or the actual trees should be returned. Defaults to "index"
#' 
#' @return A list with an element containing the the different tree clusters (as multiPhylo)
#' and the absolute, cumulative, and relative frequencies of each topology in the tree sample.
#'
#' @details This function can be used e.w. with a posterior sample of trees from a bayesian analysis where we
#' want to explore the distribution of topologies in the posterior of trees. This way we can assess topological
#' uncertainty in a more meaningful way than using a majority-rule consensus.
#' 
#' @examples
#' # tests
#' set.seed(1)
#' trl <- ape::rmtree(10, 4)
#' tpf <- topoFreq(ape::unroot(trl), output="trees")
#'
#' @export
#' @importFrom ape dist.topo
#' @importFrom stats cutree hclust

topoFreq <- function(mphy, output="index") {
    # calculate RF pairwise distance
    rfdist <- dist.topo(mphy)
    # calculate a hierarchical cluster using the distance matrix above
    hc <- hclust(rfdist)
    # use a threshold for determining clusters of topologies
    clusters <- cutree(hc, h=0.0001)
    # iterate over the cluster codes (1..max(clusters)) and populate the positions in the list
    clelements <- list()
    trclusters <- list()    
    for (i in 1:max(clusters)) {
        cl <- which(clusters == i)
        clelements <- c(clelements, list(cl))
        trclusters <- c(trclusters, list(mphy[cl]))
    }
    freqs_abs <- sapply(X = clelements, FUN = length)
    freqs_rel <- freqs_abs/length(mphy)
    if (output == "index") {
        return(list(index = clelements, fabs = freqs_abs, frel = freqs_rel))
    } else {
        return(list(trees = trclusters, fabs = freqs_abs, frel = freqs_rel))
    }
}
