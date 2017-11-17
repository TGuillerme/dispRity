#' @title Separating data into custom subsets.
#' @aliases cust.series custom.series cust.subsets
#'
#' @description Splits the data into a customized subsets list.
#'
#' @param data A \code{matrix}.
#' @param group Either a \code{list} of row numbers or names to be used as different groups or a \code{data.frame} with the same \eqn{k} elements as in \code{data} as rownames.
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
#' @seealso \code{\link{time.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}, \code{\link{crown.stem}}.
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
    check.class(data, 'matrix')

    ## data must have rownames
    if(is.null(rownames(data))) {
        warning(paste("Rownames generated for ", match_call$data, " as seq(1:", nrow(data) ,")", sep = ""))
        rownames(data) <- seq(1:nrow(data))
    } 

    ## group
    ## group is a matrix or a data.frame
    if(class(group) == "matrix" | class(group) == "data.frame") {
        group_class <- "data.frame"
        group <- as.data.frame(group)
    } else {
        if(class(group) == "list") {
            group_class <- "list"
        } else {
            stop("group argument must be either a 'list', a 'matrix' or a 'data.frame'.")
        }
    }

    ## ----------------------
    ##  SPLITING THE DATA INTO A LIST
    ## ----------------------
    if(group_class == "data.frame") {
        ## Using a data.frame or a matrix

        ## must have the same labels as data
        if(!all( as.character(rownames(group)) %in% as.character(rownames(data)))) stop("Row names in data and group arguments don't match.")

        ## Checking if the groups have a list three elements
        if(any(apply(group, 2, check.elements.data.frame))) stop("There must be at least three elements for each subset.")

        ## Creating the subsets
        subsets_list <- unlist(apply(group, 2, split.elements.data.frame, data), recursive = FALSE)
    } else {
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
            if(max(unlist(group[group_select])) > nrow(data)) stop("Row numbers in group don't match the row numbers in data.")
        } else {
            if(all(unique(unlist(lapply(group[group_select], class))) == "character")) {
                if(!all( as.character(unlist(group[group_select])) %in% as.character(rownames(data)))) stop("Row names in data and group arguments don't match.")
                
                ## Convert the row names into row numbers
                group <- lapply(group, convert.name.to.numbers, data)

            } else {
                stop("group argument must be a list of row names or row numbers.")
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