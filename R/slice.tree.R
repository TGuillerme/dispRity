#' @title Time slicing a tree.
#'
#' @usage slice.tree(tree, age, model, FAD, LAD)
#' 
#' @description Time slicing through a phylogenetic tree.
#'
#' @param tree A \code{phylo} object with a \code{root.time} element.
#' @param age A single \code{numeric} value indicating where to perform the slice.
#' @param model One of the following models: \code{"acctran"}, \code{"deltran"}, \code{"random"}, \code{"proximity"}, \code{"equal.split"} or \code{"gradual.split"}. Is ignored if \code{method = "discrete"}. See \code{\link{chrono.subsets}} for the models description.
#' @param FAD,LAD The first and last occurrence data.
#' @param keep.all.ancestors Optional, whether to also include the ancestors of the tree slice (\code{TRUE}) or just the ones linking the elements present at the slice (\code{FALSE}; default)
#' 
#' @seealso \code{paleotree::timeSliceTree}, \code{\link{chrono.subsets}}.
#'
#' @examples
#' set.seed(1)
#' ## Generate a random ultrametric tree
#' tree <- rtree(20)
#' 
#' ## Add some node labels
#' tree$node.label <- letters[1:19]
#' 
#' ## Add its root time
#' tree$root.time <- max(tree.age(tree)$ages)
#' 
#' ## Slice the tree at age 1.5
#' tree_slice <- slice.tree(tree, age = 1.5, "deltran")
#'
#' ## The slice at age 0.5 but keeping all the ancestors
#' deep_slice <- slice.tree(tree, age = 0.5, "deltran",
#'                             keep.all.ancestors = TRUE)
#'
#' ## Visualising the trees
#' old_par <- par(mfrow = c(2,2))
#' plot(ladderize(tree), main = "full tree"); axisPhylo()
#' abline(v =  tree$root.time - 1.5)
#' plot(ladderize(tree_slice), main = "tree slice"); axisPhylo()
#' plot(ladderize(deep_slice), main = "slice with ancestors"); axisPhylo()
#'
#' par(old_par)
#'
#' @author Thomas Guillerme
# @export
#' 
#' @references
#' Guillerme T. & Cooper N. \bold{2018}. Time for a rethink: time sub-sampling methods in disparity-through-time analyses. Palaeontology. DOI: 10.1111/pala.12364.
#' 

# DEBUG
# warning("DEBUG slice.tree")
# source("sanitizing.R")

#Function modified from paleotree::timeSliceTree
slice.tree <- function(tree, age, model, FAD, LAD, keep.all.ancestors = FALSE) {

    #For adding modules (i.e. models) follow the format
    # tree_slice<-timeSliceTree(tree, age, drop.extinct = TRUE, plot = FALSE)
    # for (tip in 1:Ntip(tree_slice)) {
    #   tree_sliced$tip.label[tip]<-module(tree, tree_slice$tip.label[tip], tree_slice)
    # }

    #SANITIZING
    check.class(tree, "phylo")
    check.class(age, c("numeric", "integer"), " must be a single numeric value.")
    check.length(age, 1, " must be a single numeric value.")
    check.class(model, "character", " must be one of the following: \"acctran\", \"deltran\", \"random\", \"proximity\", \"equal.split\" or \"gradual.split\".")
    model <- tolower(model)
    check.method(model, c("acctran", "deltran", "random", "proximity", "equal.split", "gradual.split"), "Slicing model")

    ## Adding a root time if missing
    if(is.null(tree$root.time)) {
        tree$root.time <- max(castor::get_all_distances_to_root(tree)[1:Ntip(tree)])
    }

    #FAD/LAD
    has_tree_age <- FALSE
    if(missing(FAD) && missing(LAD)) {
        tree_age <- tree.age(tree)
        FAD <- LAD <- tree_age
        has_tree_age <- TRUE
    } else {
        if(missing(FAD)) {
            tree_age <- tree.age(tree)
            FAD <- tree_age
            has_tree_age <- TRUE
        }
        if(missing(LAD)) {
            tree_age <- tree.age(tree)
            LAD <- tree_age
            has_tree_age <- TRUE
        }
    }

    #SLICING A TREE
    #Creating the tree.age matrix
    if(!has_tree_age) {
        tree_age <- tree.age(tree)
    }

    ## Making a sharp tree slice
    if(age > max(tree_age[,1])) {
        ## Don't slice the tree if age is too old
        return(NA)
    } else {
        tree_slice <- slice.tree.sharp(tree, age, keep.all.ancestors = keep.all.ancestors)
        if(is.null(tree_slice)) {
            return(slice.edge(tree, age, model))
        }
    }

    
    if(model == "gradual.split" || model == "equal.split") {
        
        ## Running the probability models
        tips_probabilities <- sapply(tree_slice$tip.label, function(X) slice.tree_PROXIMITY(tree, X, tree_slice, probability = TRUE), simplify = FALSE)
        tree_sliced <- do.call(rbind, tips_probabilities)

        ## Removing the rownames
        rownames(tree_sliced) <- NULL

        ## Adjusting the FADLAD
        adjust.prob <- function(one_proba, FAD, LAD, age) {
            ## If age is lower (more recent) than FAD of descendant, set probability of ancestor to 0
            if(age < FAD[which(FAD[,2] == one_proba[2]), 1]) {
                one_proba[3] <- "0"
            }
            ## If age is higher (more ancient) than LAD of ancestor, set probability of ancestor to 1
            if(age > LAD[which(LAD[,2] == one_proba[1]), 1]) {
                one_proba[3] <- "1"
            }
            return(one_proba)
        }
        tree_sliced <- t(apply(tree_sliced, 1, adjust.prob, FAD, LAD, age))

        ## Correcting probabilities if gradual.split
        if(model == "equal.split") {
            tree_sliced[,3] <- sapply(tree_sliced[,3], function(X) ifelse(round(as.numeric(X), digits = 5) == 0 || round(as.numeric(X), digits = 5) == 1, X, "0.5"))
        }

    } else {

        ## Running the discrete models
        tree_sliced <- tree_slice

        #Correcting the sliced tree
        for (tip in 1:Ntip(tree_slice)) {

            #Check if the tree is not sliced at the exact age of a tip (e.g. time=0)
            if(tree_age[which(tree_age[, 2] == tree_slice$tip.label[tip]), 1] != age) {

                #Check if the age of the tip is not in between the FAD/LAD
                if(!FAD[which(FAD[, 2] == tree_slice$tip.label[tip]),1] >= age & LAD[which(LAD[, 2] == tree_slice$tip.label[tip]), 1] <= age) {

                    #Chose the tip/node following the given model
                    selected_model <- ifelse(model == "random", sample(c("deltran", "acctran"), 1), model)

                    ## Run the slicing
                    switch(selected_model,
                        deltran = {
                            #Parent
                            tree_sliced$tip.label[tip] <- slice.tree_DELTRAN(tree, tree_slice$tip.label[tip], tree_slice)
                        },
                        acctran = {
                            #Offspring
                            tree_sliced$tip.label[tip] <- slice.tree_ACCTRAN(tree, tree_slice$tip.label[tip], tree_slice)
                        },
                        proximity = {
                            #Closest
                            tree_sliced$tip.label[tip] <- slice.tree_PROXIMITY(tree, tree_slice$tip.label[tip], tree_slice)
                        }
                    )
                }
            } 
        }
    }

    return(tree_sliced)

}