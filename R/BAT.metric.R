#' @title Using metrics from the BAT package.
#'
#' @description An interface to use metrics from the \code{BAT} package in \code{dispRity}
#'
#' @param matrix A data matrix.
#' @param BAT.fun The name of the metric or directly it's function.
#' @param BAT.args Any named optional arguments to be passed to \code{BAT.metric} (default is \code{NULL})
#'
#' @details
#' This function is an interface between the \code{dispRity} and B\code{BAT}AT package allowing to use a \code{BAT} function in the \code{dispRity} pipeline.
#' By default the function uses the \code{dispRity.metric} format:
#'
#' \code{metric_value <- BAT.metric(matrix, BAT.fun = BAT::alpha)}
#'
#' or
#'
#' \code{metric_value <- BAT.metric(matrix, BAT.fun = "alpha")} 
#'
#' or
#'
#' \code{metric_value <- BAT.metric(matrix, BAT.fun = alpha)} 
#'
#' With any optional argument being handled normally through \code{BAT.args}.
#'
#' However, most commonly, the function can be used in the context of the \code{dispRity} pipeline as follows:
#'
#' \code{my_disparity <- dispRity(my_data, metric = BAT.metric, BAT.fun = BAT::alpha)}
#'
#' \emph{NOTE} that if the \code{dispRity} object contains a \code{$tree} element. It is recycled to the BAT metric if available as an optional argument.
#' You can override this behaviour by providing your own tree (e.g. using \code{BAT.args = list(tree = my_tree)}) or by removing the tree from your input data (e.g. using \code{remove.tree(my_data)}).
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
#'
#' @seealso \code{\link{dispRity}}, \code{\link{custom.subsets}}
#' 
#' @author Thomas Guillerme
BAT.metric <- function(matrix, BAT.fun, BAT.args = NULL) {
    
    #SANITIZNG
    match_call <- match.call()

    ## Checking the matrix
    check.class(matrix, "matrix")

    ## Checking the fun
    BAT.fun_class <- check.class(BAT.fun, c("function", "character"))
    if(BAT.fun_class == "function") {
        ## Check if the function arguments look like BATlike
        if(!("comm" %in% names(formals(BAT.fun)))) {
            stop.call(msg.pre = "The function ", call = match_call$BAT.fun, msg = " doesn't look like it's formatted in the BAT style (missing the \"comm\" argument).")
        }
    } else {
        ## Use the implemented functions
        recognised_function_names <- c("alpha")
        ## Check if the method exist and is unambiguous
        check.method(BAT.fun, all_arguments = recognised_function_names, msg = "BAT.fun must be a function or")
        ## Replace the method
        BAT.fun <- eval(str2lang(paste0("BAT::", BAT.fun)))
    }

    ## Check if the function needs a tree
    dendro_tree_arg <- names(formals(BAT.fun))[2]

    ## Handle the comms arg
    comm_arg <- NULL
    ## Basic
    comm_arg <- make.BAT.comm(matrix)

    ## Handle the arguments
    if(!is.null(BAT.args)) {
        BAT_args <- BAT.args
    } else {
        BAT_args <- list()
    }

    ## Add the comm argument
    BAT_args$comm <- comm_arg

    ## Run the fun!
    return(do.call(BAT.fun, BAT_args)[[1]])
}


