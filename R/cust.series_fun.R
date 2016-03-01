#Spliting element function
split.elements <- function(X, Y, data) {
    series_list<-list()
    for(series in 1:length(levels(as.factor(X)))) {
        selected_elements<-rownames(Y)[which(as.character(X) == as.character(levels(as.factor(X))[[series]]))]
        series_list[[series]]<-data[selected_elements,]
    }
    ## Adding names to the list (the levels of the custom series)
    names(series_list)<-levels(as.factor(X))
    ## Output
    return(series_list)
}