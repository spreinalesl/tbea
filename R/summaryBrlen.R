#' summaryBrlen: Summarise branch lengths on trees with identical topology
#' 
#' @param mphy An list of objects of class multiPhylo. If a single object in this
#' argument is of class multiPhylo, it is first enclosed in a list.
#' @param method A character with the function name for the summary to be applied
#' 
#' @return A tree of class phylo with summary branch lengths in tree$edge.length.
#'
#' @details This function can be used on the output of topofreq from the $trees element
#' in order to summarise the branch length on each topology set so that we have a single
#' tree summarising both topology and branch lengths. Useful for depicting posterior
#' tree density. Alternatively, it can be used with a single element provided that it is
#' first enclosed in a list
#' 
#' @examples
#' \donttest{
#' set.seed(1)
#' library(ape)
#' trl <- ape::rmtree(10, 4)
#' tpf <- topoFreq(unroot(trl), output="trees")
#' sumtrees <- summaryBrlen(tpf$trees, method = "median")
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow=c(1,3))
#' plot(sumtrees[[1]], type="unrooted", show.node.label=FALSE, cex=1.5)
#' plot(sumtrees[[2]], type="unrooted", show.node.label=FALSE, cex=1.5)
#' plot(sumtrees[[3]], type="unrooted", show.node.label=FALSE, cex=1.5)
#' par(oldpar)
#' }
#' 
#' @export
#' @importFrom ape root unroot makeNodeLabel

summaryBrlen <- function(mphy, method) {    
    # branch lengths are comparable and the root is given a fake branch length 0.0
    # here we only need to take the first tree as reference
    # and calculate the mean/median of the vector of brlens
    # if mphy is a single multiphylo, encapsulate it in a list
    if (inherits(mphy, "multiPhylo")) {
        mphy <- list(mphy)
    }
    # pick an arbitrary branch for rooting
    outgroup <- mphy[[1]][[1]]$tip.label[1]
    summary_trees <- vector("list", length=length(mphy))
    iter <- 1
    for (i in mphy) {
        rooted_tr <- root(i, outgroup=outgroup, resolve.root=TRUE)
        for (k in seq_along(rooted_tr)) {
            rooted_tr[[k]] <- makeNodeLabel(rooted_tr[[k]], method="md5sum")
        }
        summary_tree <- rooted_tr[[1]]
        brlens <- matrix(nrow=length(rooted_tr[[1]]$edge.length), ncol=length(rooted_tr))
        brlen_summary <- vector(length = length(rooted_tr[[1]]$edge.length))
        for (j in seq_along(rooted_tr)) {
            brlens[, j] <- rooted_tr[[j]]$edge.length
        }
        brlen_summary <- apply(X = brlens, MARGIN = 1, FUN = method)
        summary_tree$edge.length <- brlen_summary
        #print(summary_tree$edge)
        #print(summary_tree$edge.length)
        summary_trees[[iter]] <- unroot(summary_tree)
        iter <- iter + 1
    }
    return(summary_trees)
}
