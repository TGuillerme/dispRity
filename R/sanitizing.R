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
check.dist.matrix <- function(matrix, method, just.check = FALSE) {

    ## Is distance
    was_dist <- FALSE

    ## Is the matrix square?
    if(dim(matrix)[1] == dim(matrix)[2] &&
       all(diag(matrix) == 0) &&
       all(matrix[upper.tri(matrix)] == matrix[rev(lower.tri(matrix))])) {

        ## It was a distance matrix!
        was_dist <- TRUE
    }

    if(just.check) {
        ## Simply return the check
        return(was_dist)
    } else {
        ## Return a matrix
        if(was_dist) {
            return(list(stats::as.dist(matrix), "was_dist" = TRUE))
        } else {
            return(list(vegan::vegdist(matrix, method = method), "was_dist" = FALSE))
        }
    }
}

## Stop with call message wrapper function
stop.call <- function(call, msg, msg.pre = "") {
    stop(paste0(msg.pre, as.expression(call), msg), call. = FALSE)
}

#     ## Is the matrix square?
#     if(dim(matrix)[1] == dim(matrix)[2]) {

#         ## Check if the diagonal is equal to 0
#         if(all(diag(matrix) == 0)) {

#             ## Check if both triangles are equal
#             if(all(matrix[upper.tri(matrix)] == matrix[rev(lower.tri(matrix))])) {
#                 return(list(stats::as.dist(matrix), "was_dist" = TRUE))
#             } else {
#                 return(list(vegan::vegdist(matrix, method = method), "was_dist" = FALSE))
#             }
#         } else {
#             return(list(vegan::vegdist(matrix, method = method), "was_dist" = FALSE))
#         }
#     } else {
#         return(list(vegan::vegdist(matrix, method = method), "was_dist" = FALSE))
#     }
# }





## Transforming a tree to binary with no zero length branches.
# bin.tree <- function(tree){
#     if(!is.binary.tree(tree)) {
#         tree <- multi2di(tree)
#         warning('tree is now binary.' , call.=FALSE)
#     }
#     ## Null branch length?
#     if(any(tree$edge.length == 0)){
#         tree$edge.length[which(tree$edge.length == 0)] <- min(tree$edge.length[-which(tree$edge.length == 0)])*0.01
#         warning('New branches length generated are set to 1% of the minimum branch length.' , call. = FALSE)
#     }
#     return(tree)
# }

## Replacing a value to be NA
# replace.na <- function(x, y="?") {
#     x[which(x == y)]  <-  NA
#     return(x)
# }

# make.nexus <- function(matrix, header, ordering, weights) {
#     ## SANITIZING
#     ## matrix
#     check.class(matrix, "matrix")

#     ## header
#     if(missing(header)) {
#         header <- NA
#     } else {
#         check.class(header, "character")
#         check.length(header, 1, " must be a single character string.", errorif=FALSE)
#     }

#     ## ordering
#     if(missing(ordering)) {
#         ordering <- rep("unord", ncol(matrix))
#     } else {
#         check.class(ordering, "character")
#         check.length(ordering, ncol(matrix), " must be the same length as the matrix.", errorif = FALSE)
#         options(warn = -1)
#         if(any(ordering != c("unord", "ord"))) {
#             stop("Ordering vector must contain only 'unord' or/and 'ord' values.")
#         }
#         options(warn = 0)
#     }

#     ## weights
#     if(missing(weights)) {
#         weights <- rep(1, ncol(matrix))
#     } else {
#         check.class(weights, "integer")
#         check.length(weights, ncol(matrix), " must be the same length as the matrix.", errorif = FALSE)
#     }

#     ## BUILD THE NEXUS OBJECT
#     nexus <- list()
#     nexus$header <- header
#     nexus$matrix <- matrix
#     nexus$ordering <- ordering
#     nexus$weights <- weights
#     nexus$max.vals <- apply(matrix, 2, max, na.rm=TRUE)
#     nexus$min.vals <- apply(matrix, 2, min, na.rm=TRUE)

#     return(nexus)
# }