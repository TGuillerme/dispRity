#FUNCTIONS FOR DISPRITY

#lapply fun
lapply_fun<-function(data, fun) {
    return(lapply(data, fun))
}

#Calculates disparity metrics
disparity.calc<-function(BSresult, class.metric, summary.metric) {
    matrix_descriptor<-lapply(BSresult, lapply_fun, fun=class.metric)
    matrix_summary<-lapply(matrix_descriptor, lapply_fun, fun=summary.metric)
    return(matrix_summary)
}
