#FUNCTIONS FOR DISPRITY

#lapply fun
lapply_fun<-function(data, fun) {
    return(lapply(data, fun))
}

# #Calculates disparity metrics
# disparity.calc<-function(BSresult, class.metric, summary.metric) {
#     matrix_descriptor<-lapply(BSresult, lapply_fun, fun=class.metric)
#     matrix_summary<-lapply(matrix_descriptor, lapply_fun, fun=summary.metric)
#     return(matrix_summary)
# }


disparity.calc<-function(BSresult, level3.fun, level2.fun, level1.fun) {
    #Run level 3 fun (matrix transformation - mat.trans)
    if(!is.null(level3.fun)) {
        matrix_decomposition <- lapply(BSresult, lapply_fun, fun=level3.fun)
    } else {
        matrix_decomposition <- BSresult
    }

    #Run level 2 fun (vector aggregate - vec.aggr)
    if(!is.null(level2.fun)) {
        matrix_decomposition <- lapply(matrix_decomposition, lapply_fun, fun=level2.fun)
    } else {
        matrix_decomposition <- matrix_decomposition
    }

    #Run level 1 fun (value aggregate - val.aggr)
    matrix_decomposition <- lapply(matrix_decomposition, lapply_fun, fun=level1.fun)

    return(matrix_decomposition)
}
