#FUNCTIONS FOR DISPRITY

#lapply fun
lapply_fun<-function(data, fun, ...) {
    return(lapply(data, fun, ...))
}

disparity.calc<-function(BSresult, level3.fun, level2.fun, level1.fun, ...) {
    #Run level 3 fun (matrix transformation - mat.trans)
    if(!is.null(level3.fun)) {
        matrix_decomposition <- lapply(BSresult, lapply_fun, fun = level3.fun, ...)
    } else {
        matrix_decomposition <- BSresult
    }

    # matrix_decomposition <- ifelse(!is.null(level3.fun), lapply(BSresult, lapply_fun, fun = level3.fun, verbose, ...), BSresult)

    #Run level 2 fun (vector aggregate - vec.aggr)
    if(!is.null(level2.fun)) {
        matrix_decomposition <- lapply(matrix_decomposition, lapply_fun, fun = level2.fun, ...)
    } else {
        matrix_decomposition <- matrix_decomposition
    }

    # matrix_decomposition <- ifelse(!is.null(level2.fun), lapply(BSresult, lapply_fun, fun = level2.fun, verbose, ...), matrix_decomposition)

    #Run level 1 fun (vector aggregate - vec.aggr)
    if(!is.null(level1.fun)) {
        matrix_decomposition <- lapply(matrix_decomposition, lapply_fun, fun = level1.fun, ...)
    } else {
        matrix_decomposition <- matrix_decomposition
    }

    # matrix_decomposition <- ifelse(!is.null(level2.fun), lapply(BSresult, lapply_fun, fun = level2.fun, verbose, ...), matrix_decomposition)

    return(matrix_decomposition)
}
