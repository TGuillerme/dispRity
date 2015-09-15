summary.dispRity<-function(data, CI=c(50,95), cent.tend=mean, recall=FALSE, rounding) {
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #must have 5 elements
    check.length(data, 5, " must be a 'dispRity' object.")
    #must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a dispRity object.")
    results<-data$disparity
    #is the data bootstrapped?   
    if(!is.na(match("bootstraps", names(data)))) {
        #must have more than one bootstrap!
        if(length(data$bootstrap[[1]][[1]]) > 1) {
            is.bootstrapped<-TRUE
        } else {
            is.bootstrapped<-FALSE
        }
    } else {
        is.bootstrapped<-FALSE
    }
    
    #CI
    #Only check if the data is bootstrapped
    if(is.bootstrapped == TRUE) {
        check.class(CI, "numeric", " must be any value between 1 and 100.")
        #remove warnings
        options(warn=-1)
        if(any(CI) < 1) {
            stop("CI must be any value between 1 and 100.")
        }
        if(any(CI) > 100) {
            stop("CI must be any value between 1 and 100.")
        }
        options(warn=0)
    }

    #cent.tend
    #Must be a function
    check.class(cent.tend, "function")
    #The function must work
    silent<-check.metric(cent.tend)

    #recall
    check.class(recall, "logical")

    #rounding
    if(missing(rounding)) {
        #Set to default (see below)
        rounding<-"default"
    } else {
        check.class(rounding, "numeric")
    }

    #Get call
    match_call<-match.call()

    #----------------------
    # TRANSFORMING THE DATA INTO A TABLE
    #----------------------

    #Unlisting the bootstrap values
    BSresults<-unlist(recursive.unlist(results), recursive=FALSE)

    #Calculating the central tendency
    results_cent<-lapply(BSresults, cent.tend)

    #Create the results table
    results_table<-as.data.frame(cbind(rep(data$series, each=length(results[[1]])), diversity.count(data[[1]]), results_cent))

    #Add columns names
    if(is.null(match_call$cent.tend)) {
        colnames(results_table)<-c("series", "n", "mean")
    } else {
        colnames(results_table)<-c("series", "n", match_call$cent.tend)
    }

    #Calculating CIs (if bootstrapped results)
    if(is.bootstrapped == TRUE) {
        #Calculate the CIs
        results_CI<-lapply(BSresults, quantile, probs=CI.converter(CI))

        #Add to the result table
        results_table<-cbind(results_table, matrix(data=unlist(results_CI), ncol=length(CI)*2, byrow=TRUE))

        #Add the CI names
        colnames(results_table)[c(4:(length(CI)*2+3))]<-names(results_CI[[1]])
    }   

    #Round the results (number of decimals = maximum number of digits in the entire)
    if(rounding == "default") {
        for(column in 3:ncol(results_table)) {
            results_table[,column]<-round(as.numeric(results_table[,column]), digit=get.digit(as.numeric(results_table[,column])))
        }
    } else {
        for(column in 3:ncol(results_table)) {
            results_table[,column]<-round(as.numeric(results_table[,column]), digit=rounding)
        }
    }


    #----------------------
    # OUTPUT
    #----------------------
    if(recall==TRUE) {
        cat(data$call, sep="\n")
    }
    return(results_table)

}
