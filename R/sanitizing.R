## SANITIZING FUNCTIONS
## Checking the class of an object and returning an error message if != class
check.class <- function(object, class, msg, errorif = FALSE, report) {
    ## Get call
    match_call <- match.call()

    ## class_object variable initialisation
    class_object <- class(object)[1]
    ## class_length variable initialisation
    length_class <- length(class)

    ## Get the report argument
    if(missing(report)) {
        report <- 1:length_class
    }

    ## Set msg if missing
    if(missing(msg)) {
        if(length_class != 1) {
            msg <- paste(" must be of class ", paste(class[report], collapse = " or "), ".", sep = "")
        } else {
            msg <- paste(" must be of class ", class, ".", sep = "")
        }
    }

    ## check if object is class.
    if(length_class != 1) {
    ## check if object is class in a cascade (class[1] else class[2] else class[3], etc..)
    ## returns error only if object is not of any class

        error <- NULL
        for(counter in 1:length_class) {
            if(errorif != TRUE) {
                if(class_object != class[counter]) {
                    error <- c(error, TRUE)
                } else {
                    error <- c(error, FALSE)
                }
            } else {
                if(class_object == class[counter]) {
                    error <- c(error, TRUE)
                } else {
                    error <- c(error, FALSE)
                }
            }
        }
        ## If function did not return, class is not matching
        if(!any(!error)) {
            stop(match_call$object, msg, call. = FALSE)
        } else {
            return(class_object)
        }

    } else {
        if(errorif != TRUE) {
            if(class_object != class) {
                stop(match_call$object, msg , call. = FALSE)
            }
        } else {
            if(class_object == class) {
                stop(match_call$object, msg , call. = FALSE)
            }        
        }
    } 
}

## Checking the class of an object and returning an error message if != class
check.length <- function(object, length, msg, errorif = FALSE) {

    match_call <- match.call()

    if(errorif != TRUE) {
        if(length(object) != length) {
            stop(match_call$object, msg , call. = FALSE)
        }
    } else {
        if(length(object) == length) {
            stop(match_call$object, msg , call. = FALSE)
        }        
    }
}

## Checking if a method matches the authorized method
check.method <- function(argument, all_arguments, msg, condition = all) {
    if(condition(is.na(match(argument, all_arguments)))) {
        stop(paste(msg, " must be one of the following: ", paste(all_arguments, collapse = ", "), ".", sep = ""), call. = FALSE)
    }
}

## Checking if a matrix is a distance matrix
check.dist.matrix <- function(matrix, method, just.check = FALSE, ...) {

    ## Is distance
    was_dist <- FALSE

    ## Check if distance
    if(is(matrix, "dist")) {
        return(list(matrix, "was_dist" = TRUE))
    }

    ## Is the matrix square?
    if(dim(matrix)[1] == dim(matrix)[2] &&
       all(diag(as.matrix(matrix)) == 0) &&
       all(matrix[upper.tri(matrix)] == matrix[rev(lower.tri(matrix))], na.rm = TRUE)) {
        ## It was a distance matrix!
        was_dist <- TRUE
    }

    if(just.check) {
        ## Simply return the check
        return(was_dist)
    } else {
        ## Return a matrix
        if(was_dist) {
            return(list(stats::as.dist(matrix), "was_dist" = TRUE))
        } else {
            return(list(vegan::vegdist(matrix, method = method, ...), "was_dist" = FALSE))
        }
    }
}

## Stop with call message wrapper function
stop.call <- function(call, msg, msg.pre = "") {
    stop(paste0(msg.pre, as.expression(call), msg), call. = FALSE)
}

## Check through a list
check.list <- function(list, check.fun, condition, ...) {
    ## Run the checks
    check_results <- lapply(list, check.fun, ...)
    ## Apply the condition
    if(!missing(condition)) {
        return(unlist(lapply(check_results, condition)))
    } else {
        return(unlist(check_results))
    }
}

## Function for test results with rounding
expect_equal_round <- function(x, y, digits) {
    testthat::expect_equal(round(x, digits), round(y, digits))
}

## Function for automatically add rownames (used in both branches below)
add.rownames <- function(x) {
    rownames(x) <- seq(1:nrow(x))
    return(x)
}

## Checks whether the data is a matrix or a list
check.data <- function(data, match_call) {

    ## Multi toggle
    is_multi <- FALSE
    
    ## Check class
    data_class <- check.class(data, c("matrix", "data.frame", "list"))

    ## If data.frame, change to matrix
    if(data_class == "data.frame") {
        data <- as.matrix(data)
    }

    ## Adding rownames
    row_warning <- paste0("Row names have been automatically added to ", as.expression(match_call$data), ".")

    ## Switch between cases
    if(data_class == "list") {
        ## Error message
        is_error <- " must be matrix or a list of matrices with the same dimensions and unique row names."
        ## Check the classes
        all_classes <- unique(unlist(lapply(data, class)))
        if(!all(all_classes %in% c("matrix", "array"))) {
            stop.call(match_call$data, is_error)
        } 
        ## Check the columns
        if(length(unique(unlist(lapply(data, ncol)))) > 1) {
            stop.call(match_call$data, is_error)
        }
        ## Check the rownames
        if(is.null(unlist(lapply(data, rownames)))) {
            ## If no rownames, add them
            data <- lapply(data, add.rownames)
            warning(row_warning)
        } 
        ## Find the rows that are not in common between matrices
        rows <- unique(unlist(lapply(data, rownames)))
        unmatch <- unlist(lapply(data, function(x, rows) return(rows[!rows %in% rownames(x)]), rows = rows))
        if(length(unmatch) > 0) {
            ## Warning rows
            warning(paste0("The following elements are not present in all matrices: ", paste0(unmatch, collapse = ", "), ". The matrices will be treated as separate trait-spaces."), call. = FALSE)
            ## Toggle multi
            is_multi <- TRUE
        } else {
            ## Reorder the rows to match the first matrix
            data <- lapply(data, function(x, order) x[order, , drop = FALSE], order = rownames(data[[1]]))
        }
    } else {
        ## Eventually add rownames
        if(is.null(rownames(data))) {
            data <- add.rownames(data)
            warning(row_warning)
        }
        data <- list(data)
    }
    return(list(matrix = data, multi = is_multi))
}
## Checks whether the tree is in the correct format
check.tree <- function(tree, data, bind.trees = FALSE, match_call) {
    
    ## multi toggle
    is_multi <- FALSE

    ## Check class
    tree_class <- check.class(tree, c("phylo", "multiPhylo"))
    ## Convert into a list (not multiPhylo if it's a single tree)
    if(tree_class == "phylo") {
        tree <- list(tree)
        class(tree) <- "multiPhylo"
    }

    ## Bind tree auto-toggle
    if(length(tree) != 1 && (length(tree) == length(data$matrix))) {
        bind.trees <- TRUE
    }

    ## Inc.nodes toggle
    inc.nodes <- unique(unlist(lapply(tree, function(x) !is.null(x$node.label))))
    if(length(inc.nodes) > 1) {
        stop("All trees should have node labels or no node labels.", call. = FALSE)
    }
    ## Double check the inc.nodes
    if(inc.nodes) {
        ## ignore node labels if no overlap
        inc.nodes <- any(unlist(mapply(function(x, y) any(rownames(x) %in% y$node.label), data$matrix, tree, SIMPLIFY = FALSE)))
    }

    ## Make the data into "dispRity" format for testing
    if(!missing(data) && is.array(data) || is.null(names(data))) {
        if(!is(data, "dispRity")) {
            data <- list(matrix = list(data))
        }
    }

    ## Match with the data
    if(!missing(data) && !is.null(data$matrix[[1]])) {
        ## Match the data and the trees?
        pass.fun <- function(cleaned) return(!all(is.na(cleaned$dropped_tips), is.na(cleaned$dropped_rows)))
        if(!bind.trees) {
            if(length(tree) != length(data$matrix)) {                
                ## Check multi trees
                dropped <- check.multi.tree(tree, data, inc.nodes)

                ## Toggle multi
                is_multi <- !all(unlist(lapply(tree, function(x, y) all(c(x$node.label, x$tip.label) %in% c(y$node.label, y$tip.label)), y = tree[[1]])))
                if(length(dropped) > 0) {
                    ## Warning
                    warning(paste0("The following elements are not present in all trees: ", paste0(dropped, collapse = ", "), ". Some analyses downstream might not work because of this (you can use ?clean.data to match both data and tree if needed)."), call. = FALSE)
                }
                ## Done for here
                return(list(tree = tree, multi = is_multi))
            } else {
                ## Normal cleaning check
                options(warn = -1)
                cleanings <- lapply(data$matrix, clean.data, tree, inc.nodes = inc.nodes)
                options(warn = 0)
            }
        } else {
            if(length(tree) != length(data$matrix)) {
                stop("The number of matrices and trees must be the same to bind them.", call. = FALSE)
            }
            ## Clean
            cleanings <- mapply(clean.data, data$matrix, tree, MoreArgs = list(inc.nodes = inc.nodes), SIMPLIFY = FALSE)
        }
        if(any(not_pass <- unlist(lapply(cleanings, pass.fun)))) {
            ## Stop!
            stop("The data is not matching the tree labels (you can use ?clean.data to match both data and tree).", call. = FALSE)
        }
    }

    return(list(tree = tree, multi = is_multi))
}
## Check the match between multiple trees and data
check.multi.tree <- function(tree, data, inc.nodes) {
    ## Get all the labels from the matrices and make a dummy one
    all_elements <- unique(unlist(lapply(data$matrix, rownames)))
    dummy_data <- matrix(1, nrow = length(all_elements), dimnames = list(all_elements))
    ## Make a dummy matrix with these
    cleanings <- lapply(tree, function(tree, data, inc.nodes) clean.data(data, tree, inc.nodes), data = dummy_data, inc.nodes = inc.nodes)
    dropped <- unique(unlist(lapply(cleanings, function(x) return(c(x$dropped_rows, x$dropped_tips)))))
    if(any(is.na(dropped))) {
        dropped <- dropped[-which(is.na(dropped))]
    }
    return(dropped)
}

## Wrapper function for checking the data
check.dispRity.data <- function(data = NULL, tree = NULL, bind.trees = FALSE, returns = c("matrix", "tree", "multi")) {

    match_call <- match.call()
    is_multi <- FALSE

    ## Checking the data
    if(!is.null(data) && !is(data, "dispRity")) {
        data <- check.data(data, match_call)
        is_multi <- any(is_multi, data$multi)
        data$multi <- NULL
    }

    ## Checking the tree
    if(!is.null(tree)) {
        tree <- check.tree(tree, data, bind.trees, match_call)
        is_multi <- any(is_multi, tree$multi)
    }

    ## Sort the output
    output <- list()
    if("matrix" %in% returns) {
        output$matrix <- data$matrix
    }
    if("tree" %in% returns) {
        output$tree <- tree$tree
    }
    if("multi" %in% returns) {
        output$multi <- is_multi
    }

    ## Output
    if(length(returns) == 1) {
        return(output[[1]])
    } else {
        return(output)
    }
}
