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
#' @param covar \code{logical}, whether to treat the metric as applied the a \code{data$covar} component (\code{TRUE}) or not (\code{FALSE}; default).
#' @param get.help \code{logical}, whether to also output the \code{dist.helper} if the metric has a \code{dist.help} argument (\code{TRUE}) or not (\code{FALSE}; default).
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
make.metric <- function(fun, ..., silent = FALSE, check.between.groups = FALSE, data.dim, tree = NULL, covar = FALSE, get.help = FALSE) {
    ## Sanitizing
    ## fun
    check.class(fun, c("function", "standardGeneric"), report = 1)
    dots <- list(...)
    fun_type <- NULL
    
    ## Getting the function name
    match_call <- match.call()

    ## Get the metric arguments
    arguments <- names(formals(fun))
    # if(length(mat_arg <- which("matrix" %in% arguments)) > 0 || length(arguments) > 1) {
    #     arguments <- arguments[-mat_arg]
    # }

    ## Detecting a between.groups and phylo arguments
    is_between.groups <- all(c("matrix", "matrix2") %in% arguments)
    is_phylo <- "tree" %in% arguments

    ## Checking for helpers
    dist.help <- help.fun <- NULL
    if(get.help) {
        get_help <- check.get.help(fun)
    } else {
        get_help <- FALSE
    }
    ## Extra check for get.help (dist.helper is an additional argument)
    if(!get_help) {
        if(!is.null(names(dots)) && any("dist.helper" %in% names(dots))) {
            get_help <- TRUE
            help.fun <- dots$dist.helper
            dots$dist.helper <- NULL
        } 
    }

    if(get_help) {

        ## Get the RAM helper
        if(is.null(help.fun)) {
            try_test <- try(help.fun <- eval(str2lang(as.character(as.expression(formals(fun)$dist.helper)))), silent = TRUE)
        } else {
            try_test <- help.fun
        }

        ## Check if the helper is a function or an object
        if(is(try_test, "function")) {

            ## Add optional arguments (if evaluable)
            if(length(optionals <- which(names(formals(try_test)) %in% arguments)) > 0) {
                help_args <- formals(fun)[optionals]
                ## Update the arguments to the dots
                if(length(arg_from_dots <- which(names(help_args) %in% names(dots))) > 0) {
                    help_args[arg_from_dots] <- dots[arg_from_dots]
                }
            } else {
                help_args <- list()
            }
            ## Add the data argument
            if(is.null(data.dim$dimensions)) {
                dims <- 1:ncol(data.dim$matrix[[1]])
            } else {
                dims <- data.dim$dimensions
            }

            ## Apply to all matrices
            get.help.matrix <- function(one_matrix, help.fun, help_args, dims) {
                help_args[[length(help_args) + 1]] <- one_matrix[, dims, drop = FALSE]
                names(help_args)[length(help_args)] <- names(formals(help.fun))[1]
                return(as.matrix(do.call(help.fun, help_args)))
            }

            ## Get the RAM help
            dist.help <- lapply(data.dim$matrix, get.help.matrix, help.fun = try_test, help_args, dims)

            ## Check if RAM help is not a dist matrix
            if(!is(dist.help[[1]], "matrix") || !check.dist.matrix(dist.help[[1]], just.check = TRUE)) {
                stop("dist.helper argument must be a distance matrix (or list of them) or a function to generate a distance matrix.", call. = FALSE)
            }
        } else {
            error <- TRUE
            if(!is(help.fun, "list")) {
                if(is(help.fun, "dist")) {
                    error <- FALSE
                    dist.help <- list(as.matrix(help.fun))
                } else {
                    if(is(help.fun, "matrix")) {
                        error <- !check.dist.matrix(help.fun, just.check = TRUE)
                        dist.help <- list(as.matrix(help.fun))
                    }
                }
            } else {
                checks <- unlist(lapply(help.fun, check.dist.matrix, just.check = TRUE))
                error <- !all(checks)
                dist.help <- lapply(help.fun, as.dist)
            }
            if(error) {
                stop("dist.helper argument must be a distance matrix (or list of them) or a function to generate a distance matrix.", call. = FALSE)
            }
        }

        ## Set the test data to be the dist.helper
        matrix <- dist.help[[1]]
        matrix_test <- ""

        if(covar) {
            matrix <- list(VCV = as.matrix(check.dist.matrix(dist.help)[[1]]), loc = diag(as.matrix(check.dist.matrix(dist.help)[[1]])))
            matrix_text <- ""
        }

        if(is_between.groups) {
            ## Create a matrix2
            matrix2 <- dist.help
            if(covar) {
                matrix2 <- list(VCV = as.matrix(check.dist.matrix(dist.help)[[1]]), loc = diag(as.matrix(check.dist.matrix(dist.help)[[1]])))
            }
        }
    } else {

        ## Simulating a matrix
        if(missing(data.dim)) {
            data.dim <- c(5, 4)
        }

        ## Tricking the simulated data if the matrix has only one dimensions
        if(data.dim[2] == 1) {
            data.dim[2] <- 2
        }
        matrix <- matrix(rnorm(data.dim[1]*data.dim[2]), data.dim[1], data.dim[2])
        matrix_text <- paste0("matrix(rnorm(",data.dim[1],"*",data.dim[2],"), ",data.dim[1], ", ",data.dim[2], ")")
        
        if(covar) {
            matrix <- list(VCV = as.matrix(dist(matrix)), loc = diag(matrix))
            matrix_text <- ""
        }

        if(is_between.groups) {
            ## Create a matrix2
            matrix2 <- matrix(rnorm(data.dim[1]*data.dim[2]), data.dim[1], data.dim[2])        
            if(covar) {
                matrix2 <- list(VCV = matrix2, loc = diag(matrix2))
            }
        }
    }

    ## Testing the metric
    test <- NULL
    op <- options(warn = -1)

    ## Skip the dots if the dots has a tree argument
    if(!is_phylo) {
        ## Test the metric
        if(is_between.groups) {
            test <- try(fun(matrix = matrix, matrix2 = matrix2, ...), silent = TRUE)
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
            test <- try(fun(matrix = matrix, matrix2 = matrix2, tree = tree, ...), silent = TRUE)
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
            return(list("type" = fun_type, "between.groups" = is_between.groups, "tree" = is_phylo, "dist.help" = dist.help))
        } else {
            return(list("type" = fun_type, "tree" = is_phylo, "dist.help" = dist.help))
        }
    } else {
        return(invisible())
    }
}