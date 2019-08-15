#' @title Reduce a matrix
#'
#' @description Reduce the number of rows/columns in a matrix to optimise overlap
#'
#' @param matrix A matrix
#' @param distance which distance to consider (passed to \code{\link[vegan]{vegdist}}, default = \code{"gower"})
#' @param by.row Whether to do it by rows (\code{TRUE} - default), or by columns (\code{FALSE})
#' @param verbose Whether to do be verbose (\code{TRUE}) or not (\code{FALSE} - default)
#' 
#' @examples
#' set.seed(1)
#' ## A 10*5 matrix
#' na_matrix <- matrix(rnorm(50), 10, 5)
#' ## Making sure some rows don't overlap
#' na_matrix[1, 1:2] <- NA
#' na_matrix[2, 3:5] <- NA
#' ## Adding 50% NAs
#' na_matrix[sample(1:50, 25)] <- NA
#' ## Illustrating the gappy matrix
#' image(t(na_matrix), col = "black")
#' 
#' ## Reducing the matrix by row
#' (reduction <- reduce.matrix(na_matrix))
#' ## Illustrating the overlapping matrix
#' image(t(na_matrix[-as.numeric(reduction$rows.to.remove), ]), col = "black")
#'
#' ## Reducing the matrix by columns (and being verbose)
#' reduce.matrix(na_matrix, by.row = FALSE, verbose = TRUE)
#' 
#' @author Thomas Guillerme
#' @export

reduce.matrix <- function(matrix, distance = "gower", by.row = TRUE, verbose = FALSE) {

    ## Sanitizing
    matrix_class <- check.class(matrix, c("matrix", "data.frame"))
    if(matrix_class == "data.frame") {
        ## Coerce into a matrix
        matrix <- as.matrix(matrix)
    }

    ## Distances
    distance_methods <- c("manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis")
    check.method(distance, distance_methods, msg = "distance")

    ## logicals
    check.class(by.row, "logical")
    check.class(verbose, "logical")

    ## Transpose the matrix if by columns
    if(!by.row) {
        matrix <- t(matrix)
    }

    ## Add row names
    if(is.null(rownames(matrix))) {
        rownames(matrix) <- 1:nrow(matrix)
    }
    ## Add col.names
    if(is.null(colnames(matrix))) {
        colnames(matrix) <- 1:ncol(matrix)
    }

    ## Remove the all NAs rows and columns
    rows_NA <- which(apply(matrix, 1, function(x) all(is.na(x))))
    cols_NA <- which(apply(matrix, 2, function(x) all(is.na(x))))

    ## Remove full NAs (rows and columns)
    if(length(rows_NA) > 0) {
        remove_rows <- -rows_NA
    } else {
        remove_rows <- 1:nrow(matrix)
    }
    if(length(cols_NA) > 0) {
        remove_cols <- -cols_NA
    } else {
        remove_cols <- 1:ncol(matrix)
    }  

    ## Reduced matrix
    no_na_input <- matrix[remove_rows, remove_cols]
    names <- rownames(no_na_input)

    ## Recording the taxa to remove (in reverse order)
    removed <- character(0)

    ## Function for selecting which row to remove
    remove.one.by.one <- function(removed, no_na_input, distance) {
        ## Get the distance matrix
        dist_matrix <- as.matrix(vegan::vegdist(no_na_input[!c(names %in% removed),], method = distance))
        
        ## Check if there are NAs
        has_NA <<- any(is.na(dist_matrix))
        
        ## Increment the rows to be removed
        if(has_NA) {
            removed <- c(names(sort(
                apply(dist_matrix, 1, function(x) length(which(is.na(x))))
                , decreasing = TRUE)[1]), removed)
        }
        return(removed)
    }

    ## Run through the first iteration
    has_NA <- TRUE
    
    ## Removing warnings for now
    options(warn = -1)

    if(verbose) message(paste0("Searching for ", ifelse(by.row, "row(s)", "column(s)"), " to remove:"), appendLF = FALSE)

    ## Looping through all the columns
    while(has_NA) {
        if(verbose) message(".", appendLF = FALSE)
        removed <- remove.one.by.one(removed, no_na_input, distance)
    }

    if(verbose) message("Done.", appendLF = TRUE)

    ## Reactivating the warnings
    options(warn = 0)

    ## Preparing the output
    if(length(removed) == 0) {
        removed <- NULL
    }
    if(length(cols_NA) == 0) {
        cols_NA <- NULL
    } else {
        cols_NA <- colnames(matrix)[cols_NA]
    }
    if(length(rows_NA) == 0) {
        rows_NA <- NULL
    } else {
        rows_NA <- rownames(matrix)[rows_NA]
    }

    if(by.row) {
        return(list("rows.to.remove" = c(removed, rows_NA), "cols.to.remove" = cols_NA))
    } else {
        return(list("rows.to.remove" = cols_NA, "cols.to.remove" = c(removed, rows_NA)))
    }
}
