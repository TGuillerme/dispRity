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

## Checks whether the data is a matrix or a list
check.dispRity.data <- function(data) {
    match_call <- match.call()
    ## Check class
    data_class <- check.class(data, c("matrix", "data.frame", "list"))

    ## If data.frame, change to matrix
    if(data_class == "data.frame") {
        data <- as.matrix(data)
    }

    ## Function for automatically add rownames (used in both branches below)
    add.rownames <- function(x) {
        rownames(x) <- seq(1:nrow(x))
        return(x)
    }
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
        ## Check the dimensions
        all_dim <- unique(unlist(lapply(data, dim)))
        if(!(length(all_dim) %in% c(1,2))) {
            stop.call(match_call$data, is_error)
        }
        ## Check the rownames
        if(is.null(unlist(lapply(data, rownames)))) {
            ## If no rownames, add them
            data <- lapply(data, add.rownames)
            warning(row_warning)
        } else {
            ## Check the rownames
            check_rows <- unique(unlist(lapply(data, rownames)))
            if(length(check_rows) != all_dim[1]) {
                stop.call(match_call$data, is_error)
            }
            ## Sort the rownames
            data <- lapply(data, function(x) x[order(rownames(x)), exact = TRUE, drop = FALSE])
        }
    } else {
        ## Eventually add rownames
        if(is.null(rownames(data))) {
            data <- add.rownames(data)
            warning(row_warning)
        }
        data <- list(data)
    }
    return(data)
}

