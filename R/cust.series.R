cust.series<-function(data, factor) {
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #data must be a matrix
    check.class(data, 'matrix')
    #data must be of size k*<=k-1
    if(ncol(data) > (nrow(data) - 1)) stop("Input data must have at least k-1 columns")

    #FACTOR
    #must be matrix or data.frame
    if(class(factor) != "matrix") {
        if(class(factor) != "data.frame") {
            stop("factor must be either a 'matrix' or a 'data.frame'.")
        }
    }
    #must have one column (if more they're ignored)
    if(ncol(factor) > 1) warning("Only the first column of 'factor' will be used.")
    #must have the same number of rows than data
    if(nrow(factor) != nrow(data)) stop('"factor" must have the same number of rows than "data".')
    #must have the same labels as data
    if(!all(sort(as.character(rownames(factor))) == sort(as.character(rownames(data))))) stop("'data' and 'factor' do not match.")
    #must have at least 3 elements per levels
    if(any(table(as.factor(factor[,1])) < 3)) stop("There must be at least three elements per series.")

    #----------------------
    # SPLITING THE DATA INTO A LIST
    #----------------------

    series_list<-list()
    for(series in 1:length(levels(as.factor(factor[,1])))) {
        selected_elements<-rownames(factor)[which(factor == levels(as.factor(factor[,1]))[[series]])]
        series_list[[series]]<-data[selected_elements,]
    }

    #Adding names to the list (the levels of the custom series)
    names(series_list)<-levels(as.factor(factor[,1]))

    return(series_list)
}