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
#' ## Basic example
#' data(demo_data)
#' BAT_data <- dispRity.BAT(demo_data$jones)
#' ## The community table
#' BAT_data$comm
#'
#' ## Complex example
#' data(disparity)
#' ## The community table for complex data
#' ## (including 100 bootstraps for 4 rarefaction levels for 7 subsets
#' ## plus the 7 raw subsets for 99 elements = 2807*99)
#' dim(dispRity.BAT(disparity)$comm)
#'
## Converts a dispRity object into BAT arguments
dispRity.BAT <- function(data, subsets, matrix, tree) {

    ## Check if data is dispRity
    check.class(data, "dispRity")
    ## Check if is subseted
    is_subseted <- length(data$subsets) != 0
    ## If so check if probabilistic
    is_proba <- FALSE
    if(is_subseted) {
        is_proba <- !is.na(data$call$subsets[2]) && grepl("split", data$call$subsets[2])
    }

    ## Placeholders
    comm <- tree <- traits <- NULL

    ## Check if matrix exists
    if(!missing(matrix)) {
        matrix <- get.matrix(data, matrix = matrix)        
    } else {
        matrix <- data$matrix[[1]]
    }

    ## Check if the data has subsets
    if(is_subseted) {
        ## Use subsets if not missing
        if(!missing(subsets)) {
            data <- get.subsets(data, subsets)
        }
       
        ## Populate the comms table
        comm <- do.call(rbind, unlist(lapply(data$subsets, lapply, make.a.subset.comm, matrix = matrix, is_proba = is_proba), recursive = FALSE))

        ## Get the table elements
        subset_names <- name.subsets(data)
        
        ## If table had only elements name them as the subsets
        if(nrow(comm) == length(subset_names)) {
            rownames(comm) <- subset_names
        } else {
            ## Specify the bootstraps and rarefactions
            names <- lapply(data$subsets, lapply, get.comm.names, matrix = matrix)
            rownames <- character()
            for(one_group in 1:length(names)) {
               rownames <- c(rownames, paste0(subset_names[one_group], ".", unname(unlist(names[[one_group]]))))
            }
            rownames(comm) <- rownames
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

## Collapse a bootstrap probability
collapse.proba <- function(proba_table) {
    ## Empty collapse table
    collapsed_matrix <- matrix(nrow = nrow(proba_table), ncol = 0)
    ## Collapse the probabilities
    while(ncol(proba_table) >= 3 && ((ncol(proba_table) %% 3) == 0)) {
        sub_table <- proba_table[, 1:3]
        collapsed_matrix <- cbind(collapsed_matrix, apply(sub_table, 1, function(x) sample(x[c(1,2)], size = 1, prob = c(x[3], 1-x[3]))))
        proba_table <- proba_table[, -c(1:3), drop = FALSE]
    }
    return(collapsed_matrix)
}

## Make a comm row out of a subset
make.a.subset.comm <- function(one_subset, matrix, is_proba = FALSE) {
    ## Get the elements
    if(is_proba) {
        elements <- collapse.proba(one_subset)
    } else {
        elements <- one_subset[,, drop = FALSE]
    }

    ## Empty community matrix
    comm_subset <- matrix(0, ncol = ncol(elements), nrow = nrow(matrix))

    ## Fill the community matrix
    for(i in 1:ncol(comm_subset)) {
        comm_subset[elements[,i],i] <- 1
    } 
    return(t(comm_subset))
}
## Get the comm names (bootstraps or rarefactions)
get.comm.names <- function(one_subset, matrix) {
    if(ncol(one_subset) == 1) {
        return("elements")
    } else {
        return(paste0("bootstrap.", nrow(one_subset), ".", 1:ncol(one_subset)))
    }
}

