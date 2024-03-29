#' @title Remove zero branch length
#'
#' @description Remove zero or negative branch lengths on trees by sliding nodes randomly in a postorder traversal based on \code{\link{slide.nodes}}.
#'
#' @param tree A \code{"phylo"} or \code{"multiPhylo"} object with edge lengths
#' @param slide An optional sliding \code{numeric} values. If left empty, 1\% of the shortest branch length is used.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#'
#' @details
#' The sliding value will be used to slide the nodes up and down to remove zero branch lengths by minimising the amount of branch changes.
#' The algorithm slides the nodes up and down (when possible) on each node in a recursive way while there is still zero or negative branch lengths.
#' If two recursions produce the same series of zero branches (e.g. by sliding node A towards node B equally so that the distance A:B becomes 0), the sliding value is divided by two until the next slide.
#' 
#' @return A \code{"phylo"} object with a postorder edge table and no zero branch lengths.
#' 
#' @examples
#' set.seed(42)
#' ## Generating a tree
#' tree <- rtree(20)
#' ## Adding some zero branch lengths (5)
#' tree$edge.length[sample(1:Nedge(tree), 5)] <- 0
#' any(tree$edge.length == 0) # TRUE
#' 
#' ## And now removing these zero branch lengths!
#' tree_no_zero <- remove.zero.brlen(tree)
#' any(tree_no_zero$edge.length == 0) # FALSE
#' 
#' ## Exaggerating the removal (to make it visible)
#' tree_exaggerated <- remove.zero.brlen(tree, slide = 1)
#' 
#' ## Plot the differences
#' par(mfrow = c(3,1))
#' plot(tree, main = "zero branch length")
#' plot(tree_no_zero, main = "no zero branch length")
#' plot(tree_exaggerated, main = "exaggerated slidding")
#' 
#' ## Removing negative branch lengths
#' ## Generating a tree with negative branch length
#' set.seed(3)
#' tree_negative <- chronoMPL(rtree(10))
#' ## Removing the negative branch length (and make it non-zero)
#' tree_positive <- remove.zero.brlen(tree_negative)
#' ## Plot the differences
#' par(mfrow = c(2, 1))
#' plot(tree_negative, main = "Negative branch lengths")
#' plot(tree_positive, main = "Positive branch lengths")
#'
#' @seealso
#' \code{\link{slide.nodes}}
#' 
#' @author Thomas Guillerme
#' @export

remove.zero.brlen <- function(tree, slide, verbose = FALSE) {
    match_call <- match.call()

    ## Tree class
    tree_class <- check.class(tree, c("phylo", "multiPhylo"))
    
    ## multiPhylo version
    if(tree_class == "multiPhylo") {
        out <- lapply(tree, remove.zero.brlen, slide, verbose)
        class(out) <- "multiPhylo"
        return(out)
    }

    ## Reorder in postorder
    tree_bkp <- tree
    tree <- reorder(tree, "postorder")

    ## Return the tree if no zero branch lengths
    if(length(which(tree$edge.length <= 0)) == 0) {
        return(tree)
    }

    ## Configure sliding
    if(missing(slide)) {
        slide <- 0.01 * min(tree$edge.length[-which(tree$edge.length <= 0)])
    } else {
        check.class(slide, c("numeric", "integer"))
        check.length(slide, 1, " must be a single numeric value.")
    }

    ## Get the root edges to see if it has a 0 length connection with a tip
    root_edges <- tree_bkp$edge[which(tree_bkp$edge[,1] == Ntip(tree) + 1),]
    ## Check if they connect to a tip
    if(any(connect_to_tip <- root_edges[,2] <= Ntip(tree))) {
        ## Check if that branch length is zero
        if(tree_bkp$edge.length[bad_edge <- which(root_edges[connect_to_tip,1] == tree_bkp$edge[,1])[connect_to_tip]] == 0) {
                stop(paste0("The root of the tree is connecting to a tip with a zero branch length: neither can be slid. You can try moving the tip manually by assigning a value to the following edge:\n    ", as.expression(match_call$tree), "$edge.length[",bad_edge ,"] <- your_value"), call. = FALSE)
        }
    }

    ## Verbose
    check.class(verbose, "logical")

    ## Sliding one node randomly
    slide.one.node <- function(node_pair, tree, slide) {
        ## Select a direction
        if(any(is.na(node_pair))) {
            ## For the direction
            if(is.na(node_pair)[2]) {
                direction <- 1
            } else {
                direction <- 2
            }
        } else {
            ## Is it a negative edge?
            edge <- which((tree$edge[,1] == node_pair[1]) & (tree$edge[,2] == node_pair[2]))
            if(tree$edge.length[edge] < 0) {
                ## Go to the right
                direction <- 2
                ## Slide is bigger
                slide <- slide - tree$edge.length[edge]
            } else {
                ## Get a random direction
                direction <- sample(c(1, 2), 1)
            }
        }

        ## Slide the node
        tree_slided <- slide.nodes.internal(tree, node_pair[direction], ifelse(direction == 1, -slide, slide), allow.negative.root = FALSE)

        ## Try reversing the slide
        if(is.null(tree_slided) && !any(is.na(node_pair))) {
            tree_slided <- slide.nodes.internal(tree, node_pair[-direction], ifelse(direction == 1, slide, -slide), allow.negative.root = FALSE)
        }
        return(tree_slided)
    }

    ## Recursive fun
    recursive.remove.zero.brlen <- function(tree, slide, max.it, verbose, zero_brlen_tracker) {

        ## Get the zero branch length
        zero_brlen <- which(tree$edge.length <= 0)

        ## Get the zero nodes
        zero_nodes <- unique(c(tree$edge[(1:nrow(tree$edge) %in% zero_brlen), 1], tree$edge[(1:nrow(tree$edge) %in% zero_brlen), 2]))

        ## Get the zero nodes surrounding a zero edge
        zero_nodes <- c(tree$edge[,1] %in% zero_nodes & tree$edge[,2] %in% zero_nodes & 1:nrow(tree$edge) %in% zero_brlen)

        ## Get the node pairs
        node_pairs <- tree$edge[zero_nodes, , drop = FALSE]

        ## Replace tips by NAs (no sliding them!)
        node_pairs <- ifelse(node_pairs <= Ntip(tree), NA, node_pairs)

        ## Replace root by NAs (no sliding them)
        node_pairs <- ifelse(node_pairs == (Ntip(tree) + 1), NA, node_pairs)

        ## Counter for avoiding infinite loops
        n_pairs <- nrow(node_pairs)
        counter <- 0

        ## Sliding down all the way
        while(counter != n_pairs) {
            ## Slide one node
            tree_test <- slide.one.node(node_pairs[1,], tree, slide)

            ## If unsuccessful slide, come to that node again
            if(is.null(tree_test)) {
                ## Move the pair of nodes at the end
                node_pairs <- rbind(node_pairs, node_pairs[1,])
            } else {
                tree <- tree_test
            }
            
            ## Verbose
            if(verbose) cat(".")

            ## Increment the counter
            counter <- counter + 1

            ## Remove pair of nodes
            node_pairs <- node_pairs[-1, , drop = FALSE]
        }

        ## Recalculate the branch lengths
        zero_brlen_tracker <- list("current" = which(tree$edge.length == 0), "previous" = zero_brlen_tracker$current, "multiplier" = zero_brlen_tracker$multiplier)
        
        if(length(zero_brlen_tracker$current) == 0) {
        
            return(tree)
        
        } else {

            ## Check if the current number of zeros is the same as previous ones
            if(length(zero_brlen_tracker$current) == length(zero_brlen_tracker$previous)) {
                ## Divide the sliding by two and add a multiplier
                slide <- slide/2
                zero_brlen_tracker$multiplier <- 2
            } else {
                ## Check if the slider was divided last time
                if(!is.null(zero_brlen_tracker$multiplier)) {
                    ## Re-multiply the slider
                    slide <- slide * 2
                    zero_brlen_tracker$multiplier <- NULL
                }
            }

            ## Recursively rerun the algorithm
            return(recursive.remove.zero.brlen(tree, slide, max.it, verbose, zero_brlen_tracker))
        }
    }

 
    ## Verbose
    if(verbose) cat(paste0("Changing ", length(which(tree$edge.length <= 0)), " branch lengths:"))

    ## Initialising the tracker
    zero_brlen_tracker <- list("current" = which(tree$edge.length <= 0))
    ## Placeholder for a max.it option
    max.it <- 1000000

    ## Remove zeros and negatives
    tree <- recursive.remove.zero.brlen(tree, slide, max.it, verbose, zero_brlen_tracker)

    if(verbose) cat("Done.")

    return(tree)
}

