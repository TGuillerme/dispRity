#' @title Using metrics from the BAT package.
#'
#' @description An interface to use metrics from the \code{BAT} package in \code{dispRity}
#'
#' @param matrix A data matrix.
#' @param tree Optional, a dendrogram or a matrix.
#' @param BAT.fun The name of the metric or directly it's function.
#' @param ... Any optional arguments to be passed to \code{BAT.metric}
#'
#' @details
#' This function is an interface between the \code{dispRity} and B\code{BAT}AT package allowing to use a \code{BAT} function in the \code{dispRity} pipeline.
#' By default the function uses the \code{dispRity.metric} format:
#'
#' \code{metric_value <- BAT.metric(matrix, BAT.fun = BAT::alpha, ...)}
#'
#' or
#'
#' \code{metric_value <- BAT.metric(matrix, BAT.fun = "alpha", ...)} 
#'
#' With any optional argument being handled normally.
#' However, most commonly, the function can be used in the context of the \code{dispRity} pipeline as follows:
#'
#' \code{my_disparity <- dispRity(my_data, metric = BAT.metric, BAT.fun = BAT::alpha, ...)}
#'
#' or
#'
#' \code{metric_value <- dispRity(my_data, metric = BAT.metric, BAT.fun = "alpha", ...)} 
#'
#' The default argument \code{tree} is handled differently depending on the context:
# TODO: contexts for tree
#' \itemize{
#'      \item if...
#'      \item if...
#'      \item if...
#' }
#' 
#' @examples
#' ## Base example:
#' ## Generating a matrix
#' dummy_matrix <- matrix(rnorm(90), 9, 10)
#' 
#' ## Applying a BAT metric to it
#' alpha_diversity <- BAT.metric(dummy_matrix, BAT.fun = "alpha")
#'
#' ## dispRity example:
#' ## Load ecological data
#' data(demo_data)
#' eco_data <- demo_data$jones
#' ## Subseted data based on two groups
#' eco_data
#' ## Apply the alpha diversity on these subsets
#' alpha_diversity <- dispRity(eco_data, metric = BAT.metric, BAT.fun = "alpha")
#' summary(alpha_diversity)
#' @seealso \code{\link{dispRity}}, \code{\link{custom.subsets}}
#' 
#' @author Thomas Guillerme
BAT.metric <- function(matrix, tree, BAT.fun, ...) {
    
    #SANITIZNG

    ## Checking the matrix
    check.class(matrix, "matrix")

    ## Checking the fun
    BAT.fun_class <- check.class(BAT.fun, c("function", "character"))
    if(BAT.fun_class == "function") {
        ## Check if the function arguments look like BATlike
    } else {
        ## Use the implemented functions
        recognised_function_names <- c("alpha")
        ## Check if the method exist and is unambiguous
        check.method(BAT.fun, all_arguments = recognised_function_names, msg = "BAT.fun must be a function or")
        ## Replace the method
        BAT.fun <- eval(str2lang(paste0("BAT::", BAT.fun)))
    }

    ## Handeling tree
    tree_arg <- NULL
    if(missing(tree)) {
        ## Check if data has a tree (this is done by dispRity at a higher level)
    } else {
        ## Check how the tree needs to be handled by BAT
        check.class(tree, c("phylo", "hclust"))
        tree_arg <- tree
    }

    ## Check if the function needs a tree
    dendro_tree_arg <- names(formals(BAT.fun))[2]

    ## Handle the comms arg
    comm_arg <- NULL
    ## Basic
    comm_arg <- make.BAT.comm(matrix)

    ## Handle BAT args
    if(!is.null(tree_arg)) {
        BAT_args <- list(comm = comm_arg, second = tree_arg, ...)
        names(BAT_args)[2] <- dendro_tree_arg
    } else {
        BAT_args <- list(comm = comm_arg, ...)
    }

    ## Run the fun!
    return(do.call(BAT.fun, BAT_args)[[1]])
}


