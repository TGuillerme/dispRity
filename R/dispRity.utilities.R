#' @name make.dispRity
#' @aliases make.dispRity fill.dispRity
#' 
#' @title Make and fill \code{dispRity}.
#' 
#' @description Creating an empty \code{dispRity} object from a matrix
#'
#' @usage make.dispRity(data, tree, call, subsets)
#' @usage fill.dispRity(data, tree, check)
#' 
#' @param data A \code{matrix}.
#' @param tree Optional, a \code{phylo} or \code{multiPhylo} object.
#' @param call Optional, a \code{list} to be a \code{dispRity} call.
#' @param subsets Optional, a \code{list} to be a \code{dispRity} subsets list.
#' @param check Logical, whether to check the data (\code{TRUE}; default, highly advised) or not (\code{FALSE}).
#' 
#' @examples
#' ## An empty dispRity object
#' make.dispRity()
#' 
#' ## Still an empty dispRity object (with a matrix)
#' (empty <- make.dispRity(data = matrix(rnorm(12), ncol = 3)))
#' 
#' ## A dispRity object with a matrix of 4*3
#' fill.dispRity(empty)
#' 
#' ## A dispRity object with a tree
#' my_tree <- rtree(4, tip.label = c(1:4))
#' fill.dispRity(empty, tree = my_tree)
#' 
#' @author Thomas Guillerme
make.dispRity <- function(data, tree, call, subsets) {
    ## Make the empty object
    dispRity_object <- list("matrix" = list(NULL) ,
                            "tree" = list(NULL),
                            "call" = list(),
                            "subsets" = list())

    ## Add the matrix
    if(!missing(data)) {
        data_class <- check.class(data, c("matrix", "list"))
        switch(data_class,
            matrix = {dispRity_object$matrix <- list(data)},
            list = {dispRity_object$matrix <- data})
    }

    ## Add the call
    if(!missing(call)) {
        check.class(call, "list")
        dispRity_object$call <- call
    }

    ## Add the tree
    if(!missing(tree)) {
        class_tree <- check.class(tree, c("multiPhylo", "phylo"))
        if(class_tree == "multiPhylo") {
            dispRity_object$tree <- tree
        } else {
            tree <- list(tree)
            class(tree) <- "multiPhylo"
            dispRity_object$tree <- tree
        }
    }

    ## Add the subsets
    if(!missing(subsets)) {
        check.class(subsets, "list")
        dispRity_object$subsets <- subsets
    }

    class(dispRity_object) <- "dispRity"

    return(dispRity_object)
}
fill.dispRity <- function(data, tree, check = TRUE) {

    ## Data have a matrix
    if(!is.null(data)) {
        if(check) {
            data$matrix <- check.dispRity.data(data$matrix, returns = "matrix")
        }

        ## Dimensions
        if(length(data$call$dimensions) == 0) {
            data$call$dimensions <- 1:ncol(data$matrix[[1]])
        }

        ## Fill empty subsets
        if(length(data$subsets) == 0) {
            data$subsets <- c(data$subsets, list(list("elements" = as.matrix(1:nrow(data$matrix[[1]])))))
        } else {
            for(subsets in 2:length(data$subsets)) {
                data$subsets[[subsets]] <- list("elements" = as.matrix(data$subsets[[subsets]]$elements))
            }
        }
    }

    if(!missing(tree)) {
        ## Add the trees
        if(check) {
            data$tree <- check.dispRity.data(tree = tree, data = data, returns = "tree")
        } else {
            data$tree <- tree
        }
    }
    return(data)
}


#' @name get.matrix
#' @aliases get.matrix get.disparity matrix.dispRity extract.dispRity
#' 
#' 
#' 
#' @title Extract elements from a \code{dispRity} object.
#' 
#' @usage get.matrix(data, subsets, rarefaction, bootstrap, matrix)
#' @usage get.disparity(data, subsets, rarefaction, observed, concatenate)
#'
#' @description Extract a matrix or the disparity results from a \code{dispRity}.
#'
#' @param data A \code{dispRity} object.
#' @param subsets Optional, a \code{numeric} or \code{character} for which subsets to get (if missing, the value for all subsets are given).
#' @param rarefaction Optional, a single \code{numeric} value corresponding to the rarefaction level (as the number of elements; if missing, the non-rarefied values are output).
#' @param bootstrap Optional, a \code{numeric} value to select a specific bootstrap draw (\code{0} is no bootstrap).
#' @param matrix A \code{numeric} value of which matrix to select (default is \code{1}).
#' @param observed A \code{logical} value indicating whether to output the observed (\code{TRUE} (default)) or the bootstrapped values (\code{FALSE}).
#' @param concatenate When the disparity metric is a distribution, whether to concatenate it returning the median (\code{TRUE}; default) or to return each individual values.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## To get the original matrix
#' get.matrix(disparity)
#' 
#' ## To get the un-bootstrapped matrix from the subset called "80"
#' get.matrix(disparity, subsets = "80")
#' 
#' ## To get the 52nd bootstrap draw of the second rarefaction level (15) of the
#' ## same subset
#' get.matrix(disparity, subsets = 2, rarefaction = 2, bootstrap = 52)
#'
#' ## Extracting the observed disparity
#' get.disparity(disparity)
#'
#' ## Extracting the bootstrapped disparity
#' boot_disp <- get.disparity(disparity, observed = FALSE)
#' str(boot_disp)
#' ## Or only the rarefied (5) data
#' boot_disp_rare <- get.disparity(disparity, observed = FALSE,
#'      rarefaction = 5)
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{get.subsets}}.
#' @author Thomas Guillerme
get.matrix <- function(data, subsets, rarefaction, bootstrap, matrix = 1){

    ## Sanitizing
    check.class(data, "dispRity")

    ## Add the dimensions if missing
    if(is.null(data$call$dimensions)) {
        data$call$dimensions <- 1:ncol(data$matrix[[1]])
    }

    if(missing(subsets)) {
        return(data$matrix[[matrix]])
    } else {
        if(missing(rarefaction) || missing(bootstrap)) {
            return(data$matrix[[matrix]][data$subsets[[subsets]]$elements, data$call$dimensions])
        } else {
            return(data$matrix[[matrix]][data$subsets[[subsets]][[rarefaction+1]][,bootstrap], data$call$dimensions])
        }
    }
}
get.disparity <- function(data, subsets, rarefaction, observed = TRUE, concatenate = TRUE) {
    #----------------------
    # SANITIZING
    #----------------------
    
    match_call <- match.call()

    ## Data
    check.class(data, "dispRity")
    ## Data must have disparity values
    if(is.null(data$call$disparity)) {
        stop.call(match_call$data, " does not contain disparity values.")
    }

    ## Observed
    check.class(observed, "logical")
    if(!observed && is.null(data$call$bootstrap)) {
        stop.call("", "Only observed values are available. Set observed = TRUE.")
    }

    ## subsets
    if(missing(subsets)) {
        subsets <- seq(1:length(data$disparity))
    } else {
        check.subsets(subsets, data)
    }

    ## Rarefaction
    if(missing(rarefaction)) {
        rarefaction <- FALSE
    }
    if(rarefaction) {
        check.class(rarefaction, c("numeric", "integer"))
        check.length(rarefaction, 1, errorif = FALSE, msg = "Only one rarefaction level can be used.")
        if(data$call$bootstrap[[3]][1] != "full" & any(is.na(match(rarefaction, data$call$bootstrap[[3]])))) {
            stop.call("", "Rarefaction level not found.")
        }
        if(observed) {
            warning("Observed value cannot be extract if rarefaction is not FALSE.")
        }
    } 

    ## Get the disparity values
    if(observed) {
        ## Lapply wrapper for getting the disparity observed values
        lapply.observed <- function(disparity) {
            return(c(apply(disparity$elements, 1, median, na.rm = TRUE)))
        }
        if(concatenate) {
            output <- lapply(data$disparity[subsets], lapply.observed)
        } else {
            output <- lapply(data$disparity[subsets], function(X) X$elements)
            ## Flatten the output matrix if 1 row
            if(all(unique(unlist(lapply(output, nrow))) == 1)) {
                output <- lapply(output, c)
            }
        }
    } else {
        output <- lapply(as.list(subsets), extract.disparity.values, data, rarefaction, concatenate)
        names(output) <- names(data$subsets[subsets])
    }

    ## Add the dimnames if necessary
    if(!data$call$disparity$metrics$between.groups) {
        return(mapply(add.dimnames, output, as.list(subsets), MoreArgs = list(data = data), SIMPLIFY = FALSE))
    } else {
        return(output)
    }
}
matrix.dispRity <- function(...) {
    warning("The function matrix.dispRity is deprecated. Use get.matrix instead.")
    return(get.matrix(...))
}
extract.dispRity <- function(...) {
    warning("The function extract.dispRity is deprecated. Use get.disparity instead.")
    return(get.matrix(...))
}










#' @name get.subsets 
#' @aliases n.subsets name.subsets size.subsets get.subsets combine.subsets
#'
#' @title Extracts or modify subsets from a \code{dispRity} object.
#' @description Extracting or modify some subsets' data and information from a \code{dispRity} object.
#' 
#' @usage n.subsets(data)
#' @usage name.subsets(data)
#' @usage size.subsets(data)
#' @usage get.subsets(data, subsets)
#' @usage combine.subsets(data, subsets)
#'
#' @param data A \code{dispRity} object.
#' @param subsets Either a \code{vector} of the number or name of the subsets to merge or a single. But see details for \code{combine.subsets}.
#'
#' @details  
#' For the function \code{combine.subsets}, the argument \code{subsets} can ALSO be a \code{numeric} value of the minimum of elements for each series.
#' If \code{subset} is a vector, the subsets are merged in the given input order. \code{c(1, 3, 4)} will merge subsets 1 and 3 into 4, while the opposite, \code{c(3, 4, 1)} will merge subsets 3 and 4 into 1.
#' When a single numeric value is given, subsets are merged with the next subset until the correct number of elements for each subset is reached (apart from the last subset that gets merged with the previous one).
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## How many subsets are in disparity?
#' n.subsets(disparity)
#'
#' ## What are the subset names
#' name.subsets(disparity)
#'
#' ## What are the number of elements per subsets?
#' size.subsets(disparity)
#' 
#' ## Get one subset
#' get.subsets(disparity, "60")
#'
#' ## Get two subsets
#' get.subsets(disparity, c(1,5))
#'
#' ## Generate subsets from a dummy matrix
#' dummy_matrix <- matrix(rnorm(120), 40, dimnames = list(c(1:40)))
#' dummy_subsets <- custom.subsets(dummy_matrix,
#'      group = list("a" = c(1:5), "b" = c(6:10), "c" = c(11:20),
#'                   "d" = c(21:24), "e" = c(25:30), "f" = c(31:40)))
#' 
#' ## Merging the two first subsets
#' combine.subsets(dummy_subsets, c(1,2))
#' 
#' ## Merging the three subsets by name
#' combine.subsets(dummy_subsets, c("d", "c", "e"))
#' 
#' ## Merging the subsets to contain at least 20 taxa
#' combine.subsets(dummy_subsets, 10)
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{get.disparity}}.
#'
#' @author Thomas Guillerme
get.subsets <- function(data, subsets) {
    ## data
    check.class(data, "dispRity")

    ## subsets
    check.subsets(subsets, data)

    ## create the new data set
    data_out <- list("matrix" = data$matrix, "call" = data$call, "subsets" = data$subsets[subsets])

    ## Add the disparity (if available)
    if(!is.null(data$call$disparity)) {
        data_out$disparity <- data$disparity[subsets]
    }
    ## Add the covar (if available)
    if(!is.null(data$covar)) {
        data_out$covar <- data$covar[subsets]
    }

    class(data_out) <- "dispRity"
    return(data_out)
}
combine.subsets <- function(data, subsets) {

    ## Internal cleaning function for only selecting the elements of the list in a subset
    select.elements <- function(subset) {
        subset[-1] <- NULL
        return(subset)
    }

    ## Saving the call
    match_call <- match.call()
    # match_call <- list(data = "data") ; warning("DEBUG combine.subsets")

    ## Sanitizing

    ## Data
    check.class(data, "dispRity")
    
    ## Check for previous data    
    has_disparity <- !is.null(data$call$disparity)
    has_bootstrap <- !is.null(data$call$bootstrap)
    if(has_disparity && has_bootstrap) {
        warning(paste(as.expression(match_call$data), "contained bootstrap and disparity data that has been discarded in the output."))
        data$disparity <- NULL
        data$call$disparity <- NULL
        data$call$bootstrap <- NULL
        data$subsets <- lapply(data$subsets, select.elements)
    } else {
        if(has_disparity && !has_bootstrap) {
            warning(paste(as.expression(match_call$data), "contained disparity data that has been discarded in the output."))
            data$disparity <- NULL
            data$call$disparity <- NULL
        } else {
            if(!has_disparity && has_bootstrap) {
                warning(paste(as.expression(match_call$data), "contained bootstrap data that has been discarded in the output."))
                data$call$bootstrap <- NULL
                data$subsets <- lapply(data$subsets, function(X){return(X[1])})
            }
        }
    }

    ## subsets
    subsets_class <- check.class(subsets, c("character", "numeric", "integer"))
    if(length(subsets) == 1 && (subsets_class == "numeric" || subsets_class == "integer")) {
        ## Subsamples is the minimum per subsets
        clean_data <- TRUE
        if(subsets > nrow(data$matrix[[1]])) {
            stop.call("", paste0("Minimum sample size (", subsets, ") cannot be greater than the number of elements in the matrix (", nrow(data$matrix[[1]]), ")."))
        }

    } else {

        if(length(data$subsets) < length(subsets)) {
            stop.call(match_call$data, " does not contain enough subsets.")
        }

        clean_data <- FALSE
        ## Must be at least two long
        if(length(subsets) < 2) {
            stop.call("", "subsets argument must contain at least two values.")
        }
        ## Must not contain duplicates
        if(length(subsets) != length(unique(subsets))) {
            stop.call("", "subsets argument must not contain duplicates.")
        }
        if(subsets_class == "character") {
            ## Must be present in the subsets names
            matches <- subsets %in% names(data$subsets)
            if(any(matches == FALSE)) {
                stop.call(msg.pre = paste0(paste(subsets[!matches], collapse = " and "), " don't match with any of the subset names in "), call = match_call$data, msg = ".")
            } else {
                subsets <- match(subsets, names(data$subsets))
            }
        } else {
            if(any(subsets > length(data$subsets))) {
                stop.call(msg.pre = paste0("subsets", paste(subsets[which(subsets > length(data$subsets))], collapse = " and "), " don't match with any of the subsets in "), call = match_call$data, msg = ".")
            }
        }
    }

    if(clean_data) {
        ## Cleaning the data
        for(subs in 1:(length(data$subsets) - 1)) {
            ## Loop oversize buffer
            if(subs > (length(data$subsets) - 1)) {break}
            ## Merging subsets
            while(nrow(data$subsets[[subs]]$elements) < subsets) {
                data <- merge.two.subsets(subs1 = subs, subs2 = (subs + 1), data = data)
            }
        }
        ## Final element
        if(nrow(data$subsets[[length(data$subsets)]]$elements) < subsets) {
            data <- merge.two.subsets(subs1 = length(data$subsets), subs2 = (length(data$subsets) - 1), data = data)
        }
        return(data)
    } else {
        ## Merging two subsets
        names <- names(data$subsets)[subsets]
        name_replace <- names(data$subsets)[subsets[length(subsets)]]
        for(subs in 1:(length(subsets)-1)) {
            ## Loop over size buffer
            if(subs > (length(data$subsets))) {break}
            ## Merging subsets
            replace_2 <- match(name_replace, names(data$subsets))
            replace_1 <- match(names[subs], names(data$subsets))
            name_replace <- paste(names[subs], name_replace, sep = "-") 
            data <- merge.two.subsets(replace_1, replace_2, data)
        }
        return(data)
    }
}
size.subsets <- function(data) {
    ## Getting the size of subsets
    return(unlist(lapply(data$subsets, function(x) nrow(x$elements))))
}
n.subsets <- function(data) {
    ## Getting the size of subsets
    return(length(data$subsets))
}
name.subsets <- function(data) {
    ## Getting the subsets names
    return(names(data$subsets))
}












#' @name add.tree
#' @aliases add.tree remove.tree get.tree
#' 
#' @title Add, remove or get trees (or subtrees)
#'
#' @usage add.tree(data, tree, replace = FALSE)
#' @usage get.tree(data, subsets = FALSE, to.root = TRUE)
#' @usage remove.tree(data)
#' 
#' @description Adding, extracting or removing the tree component from a \code{dispRity} object.
#'
#' @details \code{get.tree} allows to extract the trees specific to each subsets.
#'
#' @param data A \code{dispRity} object.
#' @param tree A \code{phylo} or \code{mutiPhylo} object.
#' @param replace Logical, whether to replace any existing tree (\code{TRUE}) or add to it (\code{FALSE}; default).
#' @param subsets Either a logical whether to extract the tree for each subset (\code{TRUE}) or not (\code{FALSE}; default) or specific subset names or numbers.
#' @param to.root Logical, whether to return the subset tree including the root of the tree (\code{TRUE}) or only containing the elements in the subset (and their most recent common ancestor; \code{FALSE}; default). If \code{data} contains time bins (from \code{\link{chrono.subsets}} with \code{method = "discrete"}), and \code{to.root = FALSE} it returns the subtrees containing only what's in the bin.
#' 
#' @examples
#' ## Loading a dispRity object
#' data(disparity)
#' ## Loading a tree
#' data(BeckLee_tree)
#' 
#' ## Removing  the tree from the dispRity object
#' (tree_data <- remove.tree(disparity))
#' 
#' ## Extracting the tree
#' get.tree(tree_data) # is null
#' 
#' ## Adding a tree to the disparity object
#' tree_data <- add.tree(tree_data, tree = BeckLee_tree)
#'
#' ## Extracting the tree
#' get.tree(tree_data) # is a "phylo" object
#'
#' ## Adding the same tree again
#' tree_data <- add.tree(tree_data, tree = BeckLee_tree)
#' get.tree(tree_data) # is a "multiPhylo" object (2 trees)
#'
#' ## Replacing the two trees by one tree
#' tree_data <- add.tree(tree_data, tree = BeckLee_tree, replace = TRUE)
#' get.tree(tree_data) # is a "phylo" object
#'
#' @seealso \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme and Jack Hadfield
add.tree <- function(data, tree, replace = FALSE) {
    ## Add the tree
    if(is.null(data$tree[[1]])) {
        data$tree <- check.dispRity.data(data = data, tree = tree, returns = "tree")
    } else {
        if(replace) {
            ## Remove existing trees
            data <- remove.tree(data)
            data <- add.tree(data, tree)
        } else {
            data$tree <- check.dispRity.data(tree = c(get.tree(data), tree), data = data, returns = "tree")
        }
    }
    return(data)
}
get.tree <- function(data, subsets = FALSE, to.root = FALSE) {
    ## Check for tree
    match_call <- match.call()
    if(is.null(data$tree)) {
        stop.call(match_call$data, " does not contain any tree(s).")
    }
    
    ## Returns just the tree
    if((is(subsets, "logical") && !subsets) || is.null(data$subsets)) {

        ## Get the tree
        tree <- data$tree

        ## Return the tree
        if(length(tree) == 1) {
            return(tree[[1]])
        } else {
            return(tree)
        }

    } else {

        ## Extract subset trees
        if((is(subsets, "logical") && subsets)) {
            ## Get all subsets
            subsets <- name.subsets(data)
        }

        ## Check the subsets names
        check.subsets(subsets, data)

        ## Check to root
        check.class(to.root, "logical")

        ## Check whether to use slicing
        slice.type <- data$call$subsets[[1]]

        ## Get the trees for each subset
        if(slice.type != "discrete") {

            ## Get the sliced trees for custom subsets
            if(slice.type == "customised") {
                trees_list <- lapply(data$subsets[subsets], get.one.tree.subset, data$tree[[1]], to.root)
            }

            ## Get the sliced trees for custom subsets
            if(slice.type == "continuous") {
                trees_list <- lapply(data$subsets[subsets], get.slice.subsets, data, to.root)
            }
            
        } else {
            bin_names <- subsets
            ## Get the bin ages
            bin_ages <- lapply(strsplit(bin_names, split = " - "), as.numeric)
            names(bin_ages) <- bin_names
            
            ## Get all the tree subsets
            all_subsets <- lapply(data$tree, get.interval.subtrees, bin_ages, to.root)

            ## Combine into multiphylo or not
            if(length(all_subsets) != 1) {
                ## Recursive merge all the trees
                while(length(all_subsets) != 1) {
                    all_subsets[[1]] <- mapply(c, all_subsets[[1]], all_subsets[[2]], SIMPLIFY = FALSE)
                    all_subsets[[2]] <- NULL
                }
            }
            ## Return the tree list
            return(all_subsets[[1]])
        }
            
        ## return the trees
        return(trees_list)
    }
}
remove.tree <- function(data) {
    ## Remove the tree
    data$tree <- list(NULL)
    return(data)
}










#' @title Rescaling and centering disparity results.
#'
#' @description Scales or/and centers the disparity measurements.
#'
#' @param data a \code{dispRity} object.
#' @param center either a \code{logical} value or a \code{numeric} vector of length equal to the number of elements of \code{data} (default is \code{FALSE}).
#' @param scale either a \code{logical} value or a \code{numeric} vector of length equal to the number of elements of \code{data} (default is \code{TRUE}).
#' @param use.all \code{logical}, whether to scale/center using the full distribution (i.e. all the disparity values) or only the distribution within each subsets of bootstraps (default is \code{TRUE}).
#' @param ... optional arguments to be passed to \code{scale}.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## Scaling the data
#' summary(rescale.dispRity(disparity, scale = TRUE)) # Dividing by the maximum
#' ## Multiplying by 10 (dividing by 0.1)
#' summary(rescale.dispRity(disparity, scale = 0.1))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{test.dispRity}}, \code{\link[base]{scale}}.
#'
#' @author Thomas Guillerme
# @export

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat50)
# groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
# customised_subsets <- custom.subsets(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 7, rarefaction = c(10, 25))
# data <- dispRity(bootstrapped_data, metric = c(sum, centroids))

# summary(data) # No scaling
# summary(rescale.dispRity(data, scale = TRUE)) # Dividing by the maximum
# summary(rescale.dispRity(data, scale = 0.1)) # Multiplying by 10
# summary(rescale.dispRity(data, center = TRUE, scale = TRUE)) # Scaling and centering
rescale.dispRity <- function(data, center = FALSE, scale = TRUE, use.all = TRUE, ...) {

    match_call <- match.call()

    ## data
    check.class(data, "dispRity")
    if(is.null(data$call$disparity)) {
        stop.call(match_call$data, "does not contain disparity values.")
    }

    ## Get the whole distribution
    all_data <- unlist(get.disparity(data))
    if(!is.null(data$call$bootstrap)) {
        all_data <- c(all_data, unlist(get.disparity(data, observed = FALSE)))
    }

    ## Getting the center value
    if(is(center, "logical")) {
        if(center & use.all) {
            center <- mean(all_data, na.rm = TRUE)
        }
    } else {
        check.class(center, c("numeric", "integer", "logical"))
        check.length(center, 1, " must be either logical or a single numeric value.")
    }

    ## Getting the scale value
    if(is(scale, "logical")) {
        if(scale & use.all) {
            scale <- max(all_data)
        }
    } else {
        check.class(scale, c("numeric", "integer", "logical"))
        check.length(scale, 1, " must be either logical or a single numeric value.")
    }

    ## Lapply functions
    lapply.scale <- function(X, center, scale) {return(t(scale(t(X), center, scale)))}

    data$disparity <- lapply(data$disparity, lapply, lapply.scale, center, scale)

    return(data)
}













#' @title Sorting or ordering a \code{dispRity} object.
#'
#' @description Sort (or order) the subsets of a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param decreasing \code{logical}. Should the sort be in ascending or descending order? Is ignored if \code{sort} is used.
#' @param sort An optional \code{vector} of \code{numeric} values corresponding to the order in which to return the subsets.
#' @param ... optional arguments to be passed to \code{sort}.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## Sorting the data
#' summary(disparity)
#' summary(sort(disparity, decreasing = TRUE))
#' summary(sort(disparity, sort = c(7,1,3,4,5,2,6)))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{test.dispRity}}, \code{\link{plot.dispRity}}, \code{\link{get.subsets}}, \code{\link{get.disparity}}.
#'
#' @author Thomas Guillerme
# @export

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# subsets <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", time = 5, model = "acctran")
# data <- dispRity(subsets, metric = mean)
# summary(data)
# summary(sort(data, decreasing = TRUE))
# summary(sort(data, sort = c(7,1,3,4,5,2,6)))
sort.dispRity <- function(x, decreasing = FALSE, sort, ...) {
    data <- x

    match_call <- match.call()

    ## Sanitizing

    ## data
    check.class(data, "dispRity")
    ## Initialising subsets length variable
    length_subsets <- length(data$subsets)
    if(length_subsets == 1) {
        stop.call(match_call$x, " contains no subsets.")
    }

    ## decreasing
    check.class(decreasing, "logical")

    ## sort
    if(!missing(sort)) {
        check.class(sort, c("numeric", "integer"))
        check.length(sort, length_subsets, " must be the same length as the number of subsets in data.")
        if(all.equal(sort(sort), seq(from = 1, to = length_subsets)) != TRUE) {
            stop.call("", paste0("The sort argument can only contain unique numbers between 1 and ", length_subsets, "."))
        }
    } else {
        if(decreasing == FALSE) sort <- seq(from = 1, to = length_subsets)
        if(decreasing == TRUE) sort <- rev(seq(from = 1, to = length_subsets))
    }


    ## Sorting the subsets
    data$subsets <- data$subsets[sort]

    ## Sorting the disparity
    if(!is.null(data$call$disparity)) {
        data$disparity <- data$disparity[sort]
    }

    ## Sorting the covar
    if(!is.null(data$covar)) {
        data$covar <- data$covar[sort]
    }

    return(data)
}












#' @title Getting the time subsets before and after an extinction event
#'
#' @description Getting the reference (pre-extinction) and the comparison (post-extinction) time subsets
#'
#' @param data a \code{dispRity} object.
#' @param extinction \code{numerical}, the time at the extinction event.
#' @param lag \code{numerical}, the lag effect (i.e. how many subsets after the extinction to consider - default = \code{1}).
#' @param names \code{logical}, whether to display the bins names (\code{TRUE}) or not (\code{FALSE} - default).
#' @param as.list \code{logical}, whether to output the results as a list for \code{\link{test.dispRity}} (\code{TRUE}) or not (\code{FALSE} - default).
#' 
#' @examples
#' ## Loading some disparity data
#' data(disparity)
#' 
#' ## Time subsets for the K-Pg extinction (66 Mya)
#' extinction.subsets(disparity, 66, names = TRUE)
#' 
#' ## Extinction with a lag effect of 3 slices
#' extinction_time <- extinction.subsets(disparity, 66, lag = 3, as.list = TRUE)
#' 
#' ## Testing the extinction effect with a lag
#' test.dispRity(disparity, wilcox.test, comparisons = extinction_time,
#'               correction = "bonferroni")
#' 
#' @seealso \code{\link{chrono.subsets}}, \code{\link{test.dispRity}}
#' 
#' @author Thomas Guillerme
# @export
extinction.subsets <- function(data, extinction, lag = 1, names = FALSE, as.list = FALSE) {

    match_call <- match.call()

    ## data
    check.class(data, "dispRity")
    if(length(data$subsets) < 2) {
        stop.call(match_call$data, " has no subsets. Use the chrono.subsets to generate some.")
    }

    ## extinction
    check.class(extinction, c("numeric", "integer"))
    check.length(extinction, 1, errorif = FALSE, msg = "extinction argument must be a single numeric argument.")
    ## check if the extinction is within the range
    data_range <- range(as.numeric(unlist(strsplit(names(data$subsets), split = " - "))))
    if(extinction < data_range[1] || extinction > data_range[2]) {
        stop.call("", paste0("extinction argument must be a numeric value between ", data_range[1], " and ", data_range[2], "."))
    }

    check.class(lag, c("numeric", "integer"))
    check.length(lag, 1, errorif = FALSE, msg = "lag argument must be a single numeric argument.")
    lag <- round(lag)
    if(lag < 1) {
        stop.call("", "lag argument must be at least 1.")
    }

    check.class(names, "logical")
    check.class(as.list, "logical")

    ## Bins or slices
    is_bins <- ifelse(data$call$subsets[1] == "discrete", TRUE, FALSE)

    if(is_bins) {
        ## Extinction bins
        extinction_subset <- grep(paste0(" - ", extinction), names(data$subsets))

        ## Check if extinction bin is in the data
        if(length(extinction_subset) == 0) {
            ## Detect the bin before the extinction time
            bin_ages <- which(detect.bin.age(data, extinction, greater = TRUE) == TRUE)
            extinction_subset<- bin_ages[length(bin_ages)]
        }
    } else{
        ## Extinction slices
        bin_ages <- which(as.numeric(names(data$subsets)) > extinction)
        extinction_subset <- bin_ages[length(bin_ages)]

    }

    ## Checking lag effect
    lag <- extinction_subset+lag
    if(lag > length(data$subsets)) {
        lag <- length(data$subsets)
        warning(paste0("Lag is too long! It was automatically set to subset number ", lag, "."))
    }
    # if(extinction == lag) {
    #     stop("No lag subset available after the extinction subset.", call. = FALSE)
    # }

    ## Adding the lag effect bins
    extinction_subset <- seq(from = extinction_subset, to = lag)

    ## Returning the names or numbers
    if(names) {
        extinction_subset <- names(data$subsets)[extinction_subset]
    }

    ## Returning a list or not
    if(as.list) {
        extinction_subset <- sapply(extinction_subset[-1], function(comp, ref) c(ref, comp), ref = extinction_subset[1], simplify = FALSE)
    }

    return(extinction_subset)

}