#' @title Separating data into custom subsets.
#' @aliases cust.series custom.series cust.subsets
#'
#' @description Splits the data into a customized subsets list.
#'
#' @param data A \code{matrix} or a \code{list} of matrices.
#' @param group Either a \code{list} of row numbers or names to be used as different groups, a \code{data.frame} with the same \eqn{k} elements as in \code{data} as rownames or a \code{factor} vector. If \code{group} is a \code{phylo} object matching \code{data}, groups are automatically generated as clades (and the tree is attached to the resulting \code{dispRity} object).
#' @param tree Optional, a \code{phylo} or \code{multiPhylo} object to attach to the resulting \code{dispRity} data.
#' 
#' @details
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

custom.subsets <- function(data, group, tree) {

    ## Saving the call
    match_call <- match.call()

    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    ## data must be a matrix
    data <- check.dispRity.data(data, returns = "data")

    ## Check whether it is a distance matrix
    if(check.dist.matrix(data[[1]], just.check = TRUE)) {
        warning("custom.subsets is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!", call. = FALSE)
    }

    ## Sanitize the group variable
    group_class <- check.class(group, c("matrix", "data.frame", "list", "phylo", "factor"))
    if(group_class == "phylo") {
        ## Saving the tree for export
        tree <- group
    }
    ## Set the group.list
    group_list <- set.group.list(group, data, group_class)

    ## Replace nulls by NAs in groups
    group_list <- lapply(group_list, function(x) if(is.null(x)){return(NA)}else{return(x)})

    ## Find empty groups
    if(any(empty_groups <- is.na(group_list))) {
        warning(paste0("The following subset", ifelse(sum(empty_groups) > 1, "s are ", " is "), "empty: ", paste0(names(which(empty_groups)), collapse = ", "), "."))
    }

    ## Check the group list
    group_list <- check.group.list(group_list, data, group_class, match_call)

    ## Make into a subset table
    subsets_list <- lapply(group_list, function(x) list(elements = matrix(x, ncol = 1)))

    ## Attach the tree
    if(group_class == "phylo" || !missing(tree)) {
        ## Output as a dispRity object (with tree)
        return(make.dispRity(data = data, call = list("subsets" = "customised"), subsets = subsets_list, tree = tree))
    } else {
        ## Output as a dispRity object
        return(make.dispRity(data = data, call = list("subsets" = "customised"), subsets = subsets_list))
    }
}