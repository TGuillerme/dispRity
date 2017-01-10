#' @title Slicing a tree.
#'
#' @usage slice.tree(tree, age, model, FAD, LAD)
#' 
#' @description Slicing through a phylogenetic tree (function modified from paleotree::timeSliceTree).
#'
#' @param tree A \code{phylo} object with a \code{root.time} element.
#' @param age A single \code{numeric} value where to perform the slice.
#' @param model One of the following models: \code{"acctran"}, \code{"deltran"}, \code{"punctuated"} or \code{"gradual"}. Is ignored if \code{method = "discrete"}.
#' @param FAD,LAD The first and last occurrence data.
#' 
#' @seealso \code{\link[paleotree]{timeSliceTree}}, \code{\link{time.series}}.
#'
#' @examples
#' set.seed(1)
#' ## Generate a random ultrametric tree
#' tree <- rcoal(20)
#' 
#' ## Add some node labels
#' tree$node.label <- letters[1:19]
#' 
#' ## Add it's root time
#' tree$root.time <- max(tree.age(tree)$ages)
#' 
#' ## Create a slice on the tree at age 0.5
#' tree_75 <- slice.tree(tree, age = 0.75, "deltran")
#'
#' @author Thomas Guillerme
#' @export


#Function modified from paleotree::timeSliceTree
slice.tree <- function(tree, age, model, FAD, LAD) {

    #For adding modules (i.e. models) follow the format
    # tree_slice<-timeSliceTree(tree, age, drop.extinct=TRUE, plot=FALSE)
    # for (tip in 1:Ntip(tree_slice)) {
    #   tree_sliced$tip.label[tip]<-module(tree, tree_slice$tip.label[tip], tree_slice)
    # }

    #SANITIZING
    #FAD/LAD
    if(missing(FAD)) {
        FAD <- tree.age(tree)
    }
    if(missing(LAD)) {
        LAD <- tree.age(tree)
    }

    #SLICING A TREE
    #Creating the tree.age matrix
    tree_age <- tree.age(tree)

    #Running the timeSliceTree function (remove warning, called as a message in the original function)
    suppressMessages(
        tree_slice <- paleotree::timeSliceTree(tree, age, drop.extinct = TRUE, plot = FALSE)
    )

    #Error with trees with two taxa
    if(Ntip(tree_slice) < 3) {
        stop('To few taxa in the tree at age ', age, '!')
    }

    #renaming the tree_slice
    tree_sliced <- tree_slice

    #Correcting the sliced tree
    for (tip in 1:Ntip(tree_slice)) {

        #Check if the tree is not sliced at the exact age of a tip (e.g. time=0)
        if(tree_age[which(tree_age[,2] == tree_slice$tip.label[tip]),1] != age) {

            #Check if the age of the tip is not in between the FAD/LAD
            if(!FAD[which(FAD[,2] == tree_slice$tip.label[tip]),1] >= age & LAD[which(LAD[,2] == tree_slice$tip.label[tip]),1] <= age) {


                #Chose the tip/node following the given model
                if(model == "punctuated") {
                    selected_model <- sample(c("deltran", "acctran"), 1)
                } else {
                    selected_model <- model
                }

                if(selected_model == "deltran") {
                    #Parent
                    tree_sliced$tip.label[tip] <- slice.tree_DELTRAN(tree, tree_slice$tip.label[tip], tree_slice)
                }

                if(selected_model == "acctran") {
                    #Offspring
                    tree_sliced$tip.label[tip] <- slice.tree_ACCTRAN(tree, tree_slice$tip.label[tip], tree_slice)
                }

                if(selected_model == "gradual") {
                    #Closest
                    tree_sliced$tip.label[tip] <- slice.tree_GRADUAL(tree, tree_slice$tip.label[tip], tree_slice)
                }              
            }
        } 
    }

    return(tree_sliced)

}