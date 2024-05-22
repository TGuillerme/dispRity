#' @title Converts \code{dispRity} to \code{BAT}.
#'
#' @description Converts a \code{dispRity} object into \code{BAT} package arguments.
#'
#' @param data A \code{matrix}, \code{data.frame} or \code{dispRity} object.
#' @param subsets Optional, some specific subsets to extract (see \code{\link{get.subsets}}).
#' @param matrix Optional, some specific matrices to extract (see \code{\link{get.matrix}}).
#' @param tree Optional, some specific trees to extract (see \code{\link{get.tree}}).
#'
#' @details
#' Converts the content of a \code{dispRity} object into a list of arguments that can be used by \code{BAT} functions.
#'
#' @return
#' \itemize{
#'      \item \code{comm}: a \code{matrix} of presence/absence (\code{0, 1}) sorting the subsets by rows and the elements by columns that can be passed as the \code{comm} argument to \code{BAT} functions. If the \code{data} contains no subsets, the matrix is matrix full of 1 with one row and a number of columns corresponding to the number of elements in \code{data}.
#'      \item \code{traits}: a \code{matrix} that is the traitspace with elements as rows and dimensions as columns.
#'      \item \code{tree}: either \code{NULL} if the \code{data} contains no tree or a \code{phylo} or \code{multiPhylo} object from \code{data}.
#' }
#' 
#' @examples
#' ## Base example:
## Converts a dispRity object into BAT arguments
dispRity.BAT <- function(data, subsets, matrix, tree) {

    ## Check if data is dispRity
    check.class(data, "dispRity")

    comm <- tree <- traits <- NULL

    ## Check if matrix exists
    if(!missing(matrix)) {
        matrix <- get.matrix(data, matrix = matrix)        
    } else {
        matrix <- data$matrix[[1]]
    }

    ## Check if the data has subsets
    if(length(data$subsets) != 0) {
        ## Use subsets if not missing
        if(!missing(subsets)) {
            data <- get.subsets(data, subsets)
        }
        ## Convert the subsets into the comm table
        comm <- matrix(0, nrow = n.subsets(data), ncol = nrow(matrix))
        rownames(comm) <- name.subsets(data)
        ## Fill up the comm table
        for(one_subset in 1:nrow(comm)) {
            ## Select the elements (can be probabilistic)
            elements <- data$subsets[[one_subset]]$elements
            if(ncol(elements) > 1) {
                elements <- apply(elements, 1, function(x) sample(x[c(1,2)], size = 1, prob = c(x[3], 1-x[3])))
            } else {
                elements <- elements[,1]
            }
            ## Se the elements in the comm matrix
            comm[one_subset, elements] <- 1
        }
    } else {
        ## No subsets
        comm <- matrix(1, nrow = 1, ncol = nrow(matrix))
    }
    colnames(comm) <- rownames(matrix)

    ## Check if the tree exist
    if(!is.null(data$tree[[1]])) {
        if(!missing(tree)) {
            if(missing(subsets)) {
                subsets <- FALSE
            }
            ## Get the tree
            tree <- get.tree(data, subsets = subsets)
            ## If the tree is multiPhylo get the first one + warning
            if(is(tree, "multiPhylo")) {
                tree <- tree[[1]]
                warning("data contained a distribution of tree. Only the first tree is used.")
            }
        }
    }

    ## Return everything
    return(list(comm   = comm,
                tree   = tree,
                traits = matrix))
}

## Transforms the trait matrix into a community one
make.BAT.comm <- function(matrix, data) {
    if(missing(data)) {
        return(matrix(1, nrow = 1, ncol = nrow(matrix)))
    } else {
        return(matrix(as.integer(rownames(data) %in% rownames(matrix)), nrow = 1, ncol = nrow(data)))
    }
}
