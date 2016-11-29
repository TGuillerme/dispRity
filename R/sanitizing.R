## SANITYZING FUNCTIONS


## Checking the class of an object and returning an error message if != class
check.class <- function(object, class, msg, errorif = FALSE) {
    ## Get call
    match_call <- match.call()

    ## class_object variable initialisation
    class_object <- class(object)
    ## class_length variable initialisation
    length_class <- length(class)

    ## Set msg if missing
    if(missing(msg)) {
        if(length_class != 1) {
            msg <- paste(" must be of class: ", paste(class, collapse = " or "), ".", sep = "")
        } else {
            msg <- paste(" must be of class: ", class, ".", sep = "")
        }
    }

    ## check if object is class.
    if(length_class != 1) {
    ## check if object is class in a cascade (class[1] else class[2] else class[3], etc..)
    ## returns error only if object is not of any class

        for(counter in 1:length_class) {
            if(errorif != TRUE) {
                if(class_object != class[counter]) {
                    return(class_object)
                }
            } else {
                if(class_object == class[counter]) {
                    return(class_object)

                }
            }
        }
        ## If function did not returned, class is not matching
        stop(match_call$object, msg, call. = FALSE)

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

## Checking if a method match the authorized method
check.method <- function(argument, all_arguments, msg) {
    if(all(is.na(match(argument, all_arguments)))) {
        stop(paste(msg, " must be one of the following: ", paste(all_methods, collapse = ", "), ".", sep = ""),  call. = FALSE)
    }
}


## Cleaning a tree so that the species match with the ones in a table
clean.tree<-function(tree, table, verbose=FALSE) {
    missing.species <- comparative.data(tree, data.frame("species"=row.names(table), "dummy"=rnorm(nrow(table)), "dumb"=rnorm(nrow(table))), "species")$dropped
    if(length(missing.species$tips) != 0) {
        tree.tmp <- drop.tip(tree, missing.species$tips)
        if (verbose==TRUE) {
            cat("Dropped tips:\n")
            cat(missing.species$tips, sep=", ")
        }
        tree <- tree.tmp
    }

    return(tree)
}

## Cleaning a table so that the species match with the ones in the tree
clean.table <- function(table, tree, verbose = FALSE) {
    missing.species <- comparative.data(tree, data.frame("species" = row.names(table), "dummy" = rnorm(nrow(table)), "dumb" = rnorm(nrow(table))), "species")$dropped
    if(length(missing.species$unmatched.rows) != 0) {
        table.tmp <- table[-c(match(missing.species$unmatched.rows, rownames(table))),]
        if (verbose == TRUE) {
            cat("Dropped rows:\n")
            cat(missing.species$unmatched.rows, sep = ", ")
        }
        table <- table.tmp
    }
    return(table)
}

## Transforming a tree to binary with no 0 branch length.
bin.tree <- function(tree){
    if(!is.binary.tree(tree)) {
        tree <- multi2di(tree)
        warning('tree is now binary.' , call.=FALSE)
    }
    ## Null branch length?
    if(any(tree$edge.length == 0)){
        tree$edge.length[which(tree$edge.length == 0)] <- min(tree$edge.length[-which(tree$edge.length == 0)])*0.01
        warning('New branches length generated are set to 1% of the minimum branch length.' , call. = FALSE)
    }
    return(tree)
}

## Replacing a value to be NA
replace.na<-function(x, y="?") {
    x[which(x == y)]  <-  NA
    return(x)
}

make.nexus <- function(matrix, header, ordering, weights) {
    ## SANITIZING
    ## matrix
    check.class(matrix, "matrix")

    ## header
    if(missing(header)) {
        header <- NA
    } else {
        check.class(header, "character")
        check.length(header, 1, " must be a single character string.", errorif=FALSE)
    }

    ## ordering
    if(missing(ordering)) {
        ordering <- rep("unord", ncol(matrix))
    } else {
        check.class(ordering, "character")
        check.length(ordering, ncol(matrix), " must be the same length as the matrix.", errorif = FALSE)
        options(warn = -1)
        if(any(ordering != c("unord", "ord"))) {
            stop("Ordering vector must contain only 'unord' or/and 'ord' values.")
        }
        options(warn = 0)
    }

    ## weights
    if(missing(weights)) {
        weights <- rep(1, ncol(matrix))
    } else {
        check.class(weights, "integer")
        check.length(weights, ncol(matrix), " must be the same length as the matrix.", errorif = FALSE)
    }

    ## BUILD THE NEXUS OBJECT
    nexus <- list()
    nexus$header <- header
    nexus$matrix <- matrix
    nexus$ordering <- ordering
    nexus$weights <- weights
    nexus$max.vals <- apply(matrix, 2, max, na.rm=TRUE)
    nexus$min.vals <- apply(matrix, 2, min, na.rm=TRUE)

    return(nexus)
}