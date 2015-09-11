dispRity<-function(data, metric, verbose=FALSE) {
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #Check if matrix is a bootstrapped list
    if(class(data) == "dispRity") {
        is.bootstraped<-TRUE
        BSresult<-data$bootstraps
        boot.call<-data$call
        taxa_list<-data$taxa
        series_list<-data$series
    } else {
        is.bootstraped<-FALSE
        #If matrix, transform to list
        if(class(data) == "matrix") {
            data<-list(data)
        }
        #Must be a list
        check.class(data, "list", " must be a matrix or a list of matrices.")
        #Each matrix must have the same number of columns
        mat_columns<-unique(unlist(lapply(data, ncol)))
        if(length(mat_columns) != 1) stop("Some matrices in data have different number of columns.")
        #Matrices must be k*<k-1 columns
        total_taxa<-unique(unlist(lapply(data, rownames)))
        if(mat_columns > (length(total_taxa) - 1)) stop("Input data must have at maximum k-1 columns")
        #Making sure there is at least 3 rows per element
        if(any(unlist(lapply(data, nrow) < 3))) stop("Some matrices in data have less than 3 rows.")

        #SIZE data
        taxa_list<-unlist(lapply(data, rownames))
        rownames(taxa_list)<-NULL
        series_list<-names(data)
        if(is.null(series_list)) {
            series_list<-length(data)
        }

        #Make the data bootstrap results format (0 bootstrap)
        BSresult<-boot.matrix(data, bootstraps=0, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full")$bootstrap

    }

    #METRIC
        # ~~~~
        # Make summary metric is not mandatory
        # ~~~~
    #must be a vector of two elements
    check.length(metric, 2, " must be a vector of two elements, the first being the metric class and the second being the metric summary.")
    #both elements must be functions
    check.class(metric[[1]], "function")
    check.class(metric[[2]], "function")
    #both functions must work
    metric1<-check.metric(metric[[1]])
    metric2<-check.metric(metric[[2]])
    #Both functions must be of different type
    if(metric1 == metric2) stop("One metric must be a class metric and the other a summary metric.")
    #Assigning each metric to it's type
    if(metric1 == "class.metric") {
        class.metric<-metric[[1]] ; summary.metric<-metric[[2]] 
    } else {
        class.metric<-metric[[2]] ; summary.metric<-metric[[1]] 
    }

    #VERBOSE
    check.class(verbose, "logical")
    # ~~~
    # Use progress bars?
    # ~~~
        #total <- 20
        ## create progress bar
        #pb <- txtProgressBar(min = 0, max = total, style = 3)
        #for(i in 1:total){
        #Sys.sleep(0.1)
        ## update progress bar
        #setTxtProgressBar(pb, i)
        #}
        #close(pb)

    #Saving the call
    match_call<-match.call()

    #----------------------
    #CALCULTING DISPARITY
    #----------------------
    #verbose
    if(verbose==TRUE) message("Calculating disparity...", appendLF=FALSE)
    #Calculate disparity in all the series
    results<-lapply(BSresult, disparity.calc, class.metric, summary.metric)
    #verbose
    if(verbose==TRUE) message("Done.", appendLF=FALSE)

    #----------------------
    #OUTPUT
    #----------------------
    #call details
    dispRity.call<-paste("Disparity calculated as: ", match_call$metric[[2]]," ", match_call$metric[[3]], " for ", ncol(BSresult[[1]][[1]][[1]]) ," dimensions.", sep="")
    #Add BS details
    if(is.bootstraped == TRUE) dispRity.call<-paste(dispRity.call, boot.call, sep="\n")


    #Creating the output object
    if(is.bootstraped == TRUE) {
        output<-list("bootstraps"=BSresult, "disparity"=results, "taxa"=taxa_list, "series"=series_list, "call"=dispRity.call)
    } else {
        output<-list("matrix"=data, "disparity"=results, "taxa"=taxa_list, "series"=series_list, "call"=dispRity.call)
    }
    #Output object is a dispRity object
    class(output)<-"dispRity"

    return(output)
}

