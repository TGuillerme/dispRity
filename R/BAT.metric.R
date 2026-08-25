#' @title Using metrics from the BAT package.
#'
#' @description An interface to use metrics from the \code{BAT} package in \code{dispRity}
#'
#' @param matrix A data matrix or a \code{BAT} structure data list (containing \code{"comm"}, \code{"tree"}, \code{"traits"}).
#' @param ... Optional variables to be passed to \code{BAT.fun}. Can be \code{tree} or \code{trait}
#' @param BAT.fun The name of the metric or directly it's function.
#' @param BAT.args Any named optional arguments to be passed to \code{BAT.metric} (default is \code{NULL})
#' @param return.all Whether to return the raw BAT results (\code{TRUE}) or not (\code{FALSE}; default)
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
BAT.metric <- function(matrix, ..., BAT.fun, BAT.args = NULL, return.raw = FALSE) {
    
    #SANITIZNG
    match_call <- match.call()

    ## Get the dots
    dots <- list(...)

    ## Checking the matrix
    input_matrix <- check.class(matrix, c("matrix", "list", "dispRity"))
    if(input_matrix == "list") {
        return(lapply(matrix, BAT.metric, BAT.fun, BAT.args, return.raw))
    }
    if(input_matrix == "dispRity") {
        stop("DEBUG BAT.metric: does not handle dispRity object yet")
        ## Needs to handle the variables as following:
        BAT::comm -> dispRity$abundance
        BAT::trait -> dispRity$matrix
        BAT::tree -> dispRity$tree
    }

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

    ## Handle the optional arguments
    if(!is.null(BAT.args)) {
        BAT_args <- BAT.args
    } else {
        BAT_args <- list()
    }

    ## Add the comm argument
    BAT_args$comm <- matrix
    warning("DEBUG BAT.metric: needs to handle matrix argument more specifically + other arguments")

    ## Run the fun!
    if(return.raw) {
        return(do.call(BAT.fun, BAT_args))
    } else {
        return(c(do.call(BAT.fun, BAT_args)))
    }
}


