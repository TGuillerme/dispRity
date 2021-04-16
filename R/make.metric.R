#' @title Creating disparity metrics
#'
#' @description Testing the dimension-level of disparity metrics
#'
#' @param fun A \code{function}.
#' @param ... Some arguments to be passed to \code{fun}.
#' @param silent \code{logical}; if \code{FALSE} (default), the function will be verbose and give no output; if \code{TRUE}, the function will only output the function's dimension-level.
#' @param check.between.groups \code{logical}; if \code{TRUE}, the function will output a named list containing the metric level and a logical indicating whether the metric can be used between groups or not. If \code{FALSE} (default) the function only outputs the metric level.
#' @param data.dim optional, two \code{numeric} values for the dimensions of the matrix to run the test function testing. If missing, a default 5 rows by 4 columns matrix is used.
#' @param tree optional, a \code{phylo} object.
#'
#' @details
#' This function tests:
#' \itemize{
#'   \item 1: if your function can deal with a matrix as an \code{input}.
#'   \item 2: which dimension-level is your function (1, 2 or 3, see \code{\link{dispRity.metric}}).
#'   \item 3: whether the function can properly be implemented in the \code{dispRity} function.
#' }
#' The three different metric levels correspond to the dimensions of the output and are:
#' \itemize{
#'   \item "dimension-level 1": for functions that decompose a \code{matrix} into a single value.
#'   \item "dimension-level 2": for functions that decompose a \code{matrix} into a \code{vector}.
#'   \item "dimension-level 3": for functions that transform the \code{matrix} into another \code{matrix}.
#' }
#' 
#' For example, the disparity metric \code{\link[base]{sum}} of \code{\link[dispRity]{variances}} is composed of two metric dimension-levels:
#' \itemize{
#'   \item The \code{\link[dispRity]{variances}} (dimension-level 2) that calculates the variances for each column in a matrix (aggregates a \code{matrix} into a \code{vector}).
#'   \item The \code{\link[base]{sum}} (dimension-level 1) that transforms the \code{vector} of variances into a single value.
#' }
#' See function example for a concrete illustration (three different dimension-levels of the function \code{\link[base]{sum}}).
#' 
#' \emph{HINT:} it is better practice to name the first argument of \code{fun} \code{matrix} to avoid potential argument conflicts down the line (the \code{\link{dispRity}} function assumes the \code{matrix} argument for the parsing the metrics).
#' 
#' The input \code{fun} can be a "normal" metric function (i.e. that takes a matrix as first argument) or a "between.groups" metric (i.e. that takes two matrix as arguments). If the arguments are named \code{matrix} and \code{matrix2}, the metric will be assumed to be "between.groups" and be run in a \code{for} loop rather than a \code{apply} loop in \code{\link{dispRity}}.
#'
#' @examples
#' ## A dimension-level 1 function
#' my_fun <- function(matrix) sum(matrix)
#' make.metric(my_fun)
#'
#' ## A dimension-level 2 function
#' my_fun <- function(matrix) apply(matrix, 2, sum)
#' make.metric(my_fun)
#'
#' ## A dimension-level 3 function
#' my_fun <- function(matrix) (matrix + sum(matrix))
#' make.metric(my_fun)
#'
#' @seealso \code{\link{dispRity}}, \code{\link{dispRity.metric}}.
#'
#' @author Thomas Guillerme
make.metric <- function(fun, ..., silent = FALSE, check.between.groups = FALSE, data.dim, tree = NULL) {
    ## Sanitizing
    ## fun
    check.class(fun, "function")
    dots <- list(...)
    fun_type <- NULL
    
    ## Getting the function name
    match_call <- match.call()

    ## Building the matrix
    if(missing(data.dim)) {
        data.dim <- c(5, 4)
    }
    matrix <- matrix(rnorm(data.dim[1]*data.dim[2]), data.dim[1], data.dim[2])
    matrix_text <- paste0("matrix(rnorm(",data.dim[1],"*",data.dim[2],"), ",data.dim[1], ", ",data.dim[2], ")")

    ## Testing the metric
    test <- NULL
    op <- options(warn = -1)

    ## Get the metric arguments
    arguments <- names(formals(fun))

    ## Detecting a between.groups and phylo arguments
    is_between.groups <- all(c("matrix", "matrix2") %in% arguments)
    is_phylo <- "tree" %in% arguments

    ## Skip the dots if the dots has a tree argument
    if(!is_phylo) {
        ## Test the metric
        if(is_between.groups) {
            test <- try(fun(matrix = matrix, matrix2 = matrix, ...), silent = TRUE)
        } else {
            test <- try(fun(matrix, ...), silent = TRUE)
        }
    } else {
        ## Build a dummy tree to match the data
        tree <- makeNodeLabel(rcoal(nrow(matrix)/2+1))
        ## Adjust the row numbers in the matrix if needed
        if((diff <- (Ntip(tree) + Nnode(tree)) - nrow(matrix)) != 0) {
            if(diff < 0) {
                ## Remove a row
                matrix <- matrix[diff,]
            } else {
                ## Add a number of rows (usually 1!)
                matrix <- rbind(matrix, matrix(rnorm(data.dim[2]*diff), nrow = diff))
            }
        }
        ## Add the rownames to the matrix to match the tree
        rownames(matrix) <- c(tree$tip.label, tree$node.label)
        ## Test the metric
        if(is_between.groups) {
            test <- try(fun(matrix = matrix, matrix2 = matrix, tree = tree, ...), silent = TRUE)
        } else {
            test <- try(fun(matrix, tree = tree, ...), silent = TRUE)
        }        
    }
    options(op)


    if(any("try-error" %in% test)){#} || any(is.na(test))) {
        if(!silent) {
            stop.call(match_call$fun, paste0("(", matrix_text, ")\nThe problem may also come from the optional arguments (...)", ifelse(is_phylo, " or the tree", " "), " in ", as.expression(match_call$fun), "."), "The provided metric function generated an error or a warning!\nDoes the following work?\n    ")
        }
    } else {

        ##########
        ## What is the output class of the function?
        ##########

        ## If class is matrix -> level3.fun
        if(is(test, "matrix")) {
            fun_type <- "level3"
            if(silent != TRUE) {
                cat(paste(as.expression(match_call$fun)," outputs a matrix object.\n", as.expression(match_call$fun), " is detected as being a dimension-level 3", ifelse(is_between.groups, " \"between.groups\" ", " "), "function.", sep = ""))
                cat(paste("\nAdditional dimension-level 2 and/or 1 function(s) will be needed.", sep = ""))
            }
        } else {
            ## If class is numeric
            if(is(test, "numeric")) {
                ## If only one value -> level1.fun
                if(length(test) == 1) {
                    fun_type <- "level1"
                    if(silent != TRUE) {
                        cat(paste(as.expression(match_call$fun)," outputs a single value.\n", as.expression(match_call$fun), " is detected as being a dimension-level 1", ifelse(is_between.groups, " \"between.groups\" ", " "), "function.", sep = ""))
                    }
                ## If more than one value -> level1.fun
                } else {
                    fun_type <- "level2"
                    if(silent != TRUE) {
                        cat(paste(as.expression(match_call$fun)," outputs a matrix object.\n", as.expression(match_call$fun), " is detected as being a dimension-level 2", ifelse(is_between.groups, " \"between.groups\" ", " "), "function.", sep = ""))
                    }
                }
            } else {
                ## Function provides a wrong output
                if(silent != TRUE) {
                    stop.call(match_call$fun, paste0(ifelse(is_between.groups, "(matrix = matrix(rnorm(20), 5,4), matrix2 = matrix(rnorm(20), 5,4))", "(matrix(rnorm(20), 5,4))"), "\nThe problem may also come from the optional arguments (...) in ", as.expression(match_call$fun), "."), "The provided metric function generated an error or a warning!\nDoes the following work?\n    ")
                } else {
                    fun_type <- "error"
                }
            }
        }
    }
    ##########
    ## Return the level type for dispRity
    ##########

    if(silent == TRUE) {
        if(check.between.groups) {
            return(list("type" = fun_type, "between.groups" = is_between.groups, "tree" = is_phylo))
        } else {
            return(list("type" = fun_type, "tree" = is_phylo))
        }
    } else {
        return(invisible())
    }
}