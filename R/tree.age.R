#' @title Calculating the age of nodes and tips in a tree.
#'
#' @description Calculates the age of each node and tip in a tree give the height of the tree or some specified age.
#'
#' @param tree A \code{phylo} object.
#' @param age The age of the tree. If missing the age is set to be the tree height.
#' @param order Either "past" if the units express time since the present (e.g. million years ago), or "present" if the unit is expressed in time since the root.
#' @param fossil \code{logical}, whether to always consider the tree as containing at least one living taxa (\code{TRUE}) or allowing only fossil taxa (\code{FALSE} - default), see details.
#' 
#' @details When \code{fossil = TRUE}, if the \code{tree} contains a \code{tree$root.time} element (for tree's root age), and that \code{order} is set to \code{"past"}, the output ages are adjusted to be starting from the root.time. Else, if no \code{tree$root.time} exists or \code{fossil = FALSE}, tips and nodes age is relative from the tip furthest away from the root.
#'
#' @examples
#' ## A dated random phylogeny with a root 50 units of time old.
#' tree.age(rtree(10), age = 50)
#' ## A random tree with the distance since the root.
#' tree.age(rtree(10), order = 'present')
#'
#' @seealso \code{\link{slice.tree}}, \code{\link{time.subsets}}.
#'
#' @author Thomas Guillerme

#Modified from [R-sig-phylo] nodes and taxa depth II - 21/06/2011 - Paolo Piras - ppiras(at)uniroma3.it

tree.age <- function(tree, age, order = 'past', fossil = TRUE){

#SANITYZING

    #tree
    check.class(tree, 'phylo', ' must be a phylo object.')

    #age
    if(missing(age)) {
    	#Using the tree height as age if age is missing
        age <- max(dist.nodes(tree)[, Ntip(tree)+1])
    }
    check.class(age, 'numeric', ' must be a numerical value.')
    check.length(age, '1', ' must a a single value.')

    #order
    check.method(order, c("past", "present"), "order argument")

    ## Fossils only
    # check.class(fossil.only, "logical")

    #CALCULATE THE EDGES AGE

    if(age != 0) {
        ages.table <- tree.age_scale(tree.age_table(tree), age)
    } else {
        ages.table <- tree.age_table(tree)
    }

    #Type
    if(order != 'past') {
        ages.table$ages <- round(ages.table$ages, digits = 7) # CHANGE PRECISION!
    } else {
        tree.height <- max(ages.table$ages)
        ages.table$ages <- round(abs(ages.table$ages - tree.height), digits = 3)
    }

    ## Adjust time for tree with non-living taxa
    if(fossil) {
        if(!is.null(tree$root.time) && order == "past") {
            ## If the root.time is not equal to the older node, scale the tree ages down
            if(round(tree$root.time, digit = 5) > round(max(ages.table$ages), digit = 5)) {
                ## Add the time to the root age to all the ages
                ages.table$ages <- ages.table$ages + abs(tree$root.time - max(ages.table$ages))
            }
        }
    }

    #Output
    #ages.table <- round(ages.table[1, ], digits = 3)
    return(ages.table)
}
