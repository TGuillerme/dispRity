## SANITIZING FUNCTIONS
## Checking the class of an object and returning an error message if != class
check.class <- function(object, class, msg, errorif = FALSE) {
    ## Get call
    match_call <- match.call()

    ## class_object variable initialisation
    class_object <- class(object)[1]
    ## class_length variable initialisation
    length_class <- length(class)

    ## Set msg if missing
    if(missing(msg)) {
        if(length_class != 1) {
            msg <- paste(" must be of class ", paste(class, collapse = " or "), ".", sep = "")
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


## Special function for checking the $matrix element
check.matrix <- function(data, count = FALSE, tree = NULL, pairwise = FALSE) {

    ## Get call and message
    match_call <- match.call()
    msg <- paste(" must be a matrix or a list of matrices.")
    stop.tree <- function(match_call) {
        stop(paste0(as.expression(match_call$data), " does not match ", as.expression(match_call$tree), ". Use the following to match both:\n    clean.data(", as.expression(match_call$data), ", ", as.expression(match_call$tree), ")" ), call. = FALSE)
    }

    ## Class check
    data_class <- check.class(data, c("matrix", "list"))

    ## Single matrix case
    if(data_class == "matrix") {
        ## Count the size
        if(count) {
            output <- list(dim(data))
        }
    } else {
        ## Check whether each elements are matrices
        all_matrix <- unlist(lapply(data, class))
        if(is.null(all_matrix) || any(all_matrix != "matrix")) {
            stop(as.expression(match_call$data), msg , call. = FALSE)
        }

        ## Count the sizes
        if(count) {
            output <- lapply(data, dim)
        }
    }

    ## Check the tree
    if(!is.null(tree)) {
        cleaned <- clean.data(data, tree, pairwise)
        if(!is.na(cleaned$dropped_rows) || !is.na(cleaned$dropped_rows)) {
            stop.tree(match_call)
        }
    }

    if(count) {
        return(output)
    } else {
        return(invisible())
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
    if(class(matrix)[1] == "dist") {
        return(list(matrix, "was_dist" = TRUE))
    }

    ## Is the matrix square?
    if(dim(matrix)[1] == dim(matrix)[2] &&
       all(diag(matrix) == 0) &&
       all(matrix[upper.tri(matrix)] == matrix[rev(lower.tri(matrix))])) {
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


