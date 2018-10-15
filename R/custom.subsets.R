#' @title Separating data into custom subsets.
#' @aliases cust.series custom.series cust.subsets
#'
#' @description Splits the data into a customized subsets list.
#'
#' @param data A \code{matrix} (see details).
#' @param group Either a \code{list} of row numbers or names to be used as different groups or a \code{data.frame} with the same \eqn{k} elements as in \code{data} as rownames. If \code{group} is a \code{phylo} object matching \code{data}, groups are automatically generated as clades.
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{matrix}{the multidimensional space (a \code{matrix}).}
#' \item{call}{A \code{list} containing the called arguments.}
#' \item{subsets}{A \code{list} containing matrices pointing to the elements present in each subsets.}
#'
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#' @details
#' The data is considered as the multidimensional space with rows as elements and columns as dimensions and is not transformed (e.g. if ordinated with negative eigen values, no correction is applied to the matrix).
#' 
#' Note that every element from the input data can be assigned to multiple groups!
#'
#' @examples
#' ## Generating a dummy ordinated matrix
#' ordinated_matrix <- matrix(data = rnorm(90), nrow = 10)
#' 
#' ## Splitting the ordinated matrix into two groups using row numbers
#' custom.subsets(ordinated_matrix, list(c(1:4), c(5:10)))
#' 
#' ## Splitting the ordinated matrix into three groups using row names
#' ordinated_matrix <- matrix(data = rnorm(90), nrow = 10,
#'      dimnames = list(letters[1:10]))
#' custom.subsets(ordinated_matrix,
#'      list("A" = c("a", "b", "c", "d"), "B" = c("e", "f", "g", "h", "i", "j"),
#'           "C" = c("a", "c", "d", "f", "h")))
#' 
#' ## Splitting the ordinated matrix into four groups using a dataframe
#' groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5), rep(c(1,2), 5)),
#'      nrow = 10, ncol = 2, dimnames = list(letters[1:10], c("g1", "g2"))))
#' custom.subsets(ordinated_matrix, groups)
#' 
#' ## Splitting a matrix by clade
#' data(BeckLee_mat50)
#' data(BeckLee_tree)
#' custom.subsets(BeckLee_mat50, group = BeckLee_tree)
#' 
#'
#' @seealso \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}, \code{\link{crown.stem}}.
#'
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG cust.subsets")
# source("sanitizing.R")
# source("custom.subsets_fun.R")
# source("dispRity.utilities.R")
# source("dispRity.utilities_fun.R")
# data <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
# group1 <- list("A" = c(1,2,3,4), "B" = c(5,6,7,8,9,10))
# group2 <- list("A" = c("a", "b", "c", "d"), "B" = c(letters[5:10]))
# group3 <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))

custom.subsets <- function(data, group) {

    ## Saving the call
    match_call <- match.call()

    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    ## data must be a matrix
    check.class(data, "matrix")

    ## Check whether it is a distance matrix
    if(check.dist.matrix(data, just.check = TRUE)) {
        warning("custom.subsets is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!", call. = FALSE)
    }

    ## data must have rownames
    if(is.null(rownames(data))) {
        warning(paste("Rownames generated for ", as.expression(match_call$data), " as seq(1:", nrow(data) ,")", sep = ""))
        rownames(data) <- seq(1:nrow(data))
    } 

    ## group
    ## group is a matrix or a data.frame
    if(class(group) == "matrix" || class(group) == "data.frame") {
        group_class <- "data.frame"
        group <- as.data.frame(group)
    } else {
        if(class(group) == "list") {
            group_class <- "list"
        } else {
            if(class(group) == "phylo") {

                group_class <- "phylo"
                ## Matching the tree and the data
                if(any(is.na(match(group$tip.label, rownames(data))))) {
                    stop.call("", "Some tips in the tree are not matching the data.\nSee ?clean.data for matching the tree and the data.")
                }

                if(is.null(group$node.label)) {
                    ## No nodes
                    if(nrow(data) != Ntip(group)) {
                        stop.call("", "Some rows in the data are not matching the tree.\nSee ?clean.data for matching the tree and the data.")
                    }
                    inc.nodes <- FALSE
                } else {
                    ## Are the nodes relevant in the data
                    if(nrow(data) == (Ntip(group) + Nnode(group))) {
                        if(any(is.na(match(group$node.label, rownames(data))))) {
                            stop.call("", "Some nodes in the tree are not matching the data.\nSee ?clean.data for matching the tree and the data.")
                        }
                        inc.nodes <- TRUE
                    } else {
                        if(nrow(data) == Ntip(group)) {
                            inc.nodes <- FALSE
                        } else {
                            stop.call("", "Some rows in the data are not matching the tree.\nSee ?clean.data for matching the tree and the data.")
                        }
                    }
                }

            } else {
                stop.call("", "group argument must be either a \"list\", a \"matrix\", a \"data.frame\" or a \"phylo\" object.")
            }
        }
    }

    ## ----------------------
    ##  SPLITING THE DATA INTO A LIST
    ## ----------------------
    if(group_class == "data.frame") {
        ## Using a data.frame or a matrix

        ## must have the same labels as data
        if(!all( as.character(rownames(group)) %in% as.character(rownames(data)))) {
            stop.call("", "Row names in data and group arguments don't match.")
        }

        ## Checking if the groups have a list three elements
        if(any(apply(group, 2, check.elements.data.frame))) {
            stop.call("", "There must be at least three elements for each subset.")
        }

        ## Creating the subsets
        subsets_list <- unlist(apply(group, 2, split.elements.data.frame, data), recursive = FALSE)
    } else {

        if(group_class == "phylo") {

            get.clade.tips <- function(node, tree, inc.nodes) {
                if(inc.nodes) {
                    clade <- extract.clade(phy = tree, node = node)
                    return(c(clade$tip.label, clade$node.label))
                } else {
                    return(extract.clade(phy = tree, node = node)$tip.label)
                }
            }

             group_tmp <- sapply(Ntip(group)+1:Nnode(group), get.clade.tips, tree = group, inc.nodes = inc.nodes, USE.NAMES = FALSE)

             if(!is.null(group$node.label)) {
                names(group_tmp) <- group$node.label
             } else {
                names(group_tmp) <- paste0("n", Ntip(group)+1:Nnode(group))
             }

             group <- group_tmp
         }

        ## Using a list

        ## Check for empty groups
        empty_groups <- is.na(group) | unlist(lapply(group, is.null))
        if(any(empty_groups)) {
            ## Prepare a warning message
            empty_groups_names <- ifelse(!is.null(names(empty_groups)), paste(names(which(empty_groups)), collapse = ", "), paste(which(empty_groups), collapse = ", "))
            being <- ifelse(length(which(empty_groups)) == 1, "is", "are")
            subset <- ifelse(length(which(empty_groups)) == 1, "Subsample", "Subsamples")
            ## Send a warning messages
            warning(paste(subset, empty_groups_names, being, "empty."))

            ## Replace NULL groups by NAs
            null_groups <- unlist(lapply(group, is.null))
            if(any(null_groups)) {
                group[which(null_groups)] <- NA
            }
        } 
        ## Select the groups for sanitising
        group_select <- which(empty_groups != TRUE)

        ## Cleaning groups
        if(all(unique(unlist(lapply(group[group_select], class))) %in% c("numeric", "integer"))) {
            ## The list must have the same columns as in the data
            if(max(unlist(group[group_select])) > nrow(data)) {
                stop.call("", "Row numbers in group don't match the row numbers in data.")
            }
        } else {
            if(all(unique(unlist(lapply(group[group_select], class))) == "character")) {
                if(!all( as.character(unlist(group[group_select])) %in% as.character(rownames(data)))) {
                    stop.call("", "Row names in data and group arguments don't match.")
                }
                
                ## Convert the row names into row numbers
                group <- lapply(group, convert.name.to.numbers, data)

            } else {
                stop.call("", "group argument must be a list of row names or row numbers.")
            }
        }

        ## Checking if the groups have names
        if(is.null(names(group))) names(group) <- seq(1:length(group))

        ## Creating the subsets
        subsets_list <- lapply(group, function(x) list("elements" = matrix(x)))
    
    }

    ## Output as a dispRity object
    return(make.dispRity(data = data, call = list("subsets" = "customised"), subsets = subsets_list))
}