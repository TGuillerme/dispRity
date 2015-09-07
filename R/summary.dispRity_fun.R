#Bootstrap unlisting
recursive.unlist<-function(results) {
    n_series<-length(results)
    n_rare<-length(results[[1]])
    n_bootstraps<-length(results[[1]][[1]])

    series_rare_boot<-list()
    for(series in 1:n_series) {
        rare_boot<-list()
        for (rare in 1:n_rare) {
            rare_boot[[rare]]<-unlist(results[[series]][[rare]])
        }
        series_rare_boot[[series]]<-rare_boot
    }
    return(series_rare_boot)
}


#Converts one or more CI into a quantile probabilities
CI.converter<-function(CI) {
    sort(c(50-CI/2, 50+CI/2)/100)
}

#Calculates taxonomic diversity per slice (include rarefaction)
diversity.count<-function(data) {
    if(class(data) == "matrix") {
        return(nrow(matrix))
    } else {
        #lapply frenzy!
        return(unlist(lapply(lapply(data, lapply, lapply, nrow), lapply, unique), use.names=FALSE))
    }
}

#Get digit for table
get.digit<-function(column) {
    if(max(nchar(round(column))) <= 4) {
        return(4-max(nchar(round(column))))
    } else {
        return(0)
    }
}