# ' @title Disparity statistics 
# '
# ' @description Applying statistical tests to dispRity objects
# '
# ' @param data A \code{dispRity} object.
# ' @param test A statistical \code{function} to apply to the data.
# ' @param format The expected input format to the function (either \code{"matrix"} or \code{"vector"} - default).
# ' @param ... Additional options to pass to the test \code{function}.
# '
# ' @examples
# ' ## Load the Beck & Lee 2014 data
# ' data(BeckLee_mat50)
# '
# ' ## Calculating the disparity from a customised series
# ' ## Generating the series
# ' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
# '      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
# '      dimnames = list(rownames(BeckLee_mat50))))
# ' customised_series <- cust.series(BeckLee_mat50, factors)
# ' ## Bootstrapping the data
# ' bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
# ' ## Caculating the sum of ranges
# ' sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))
# '
# ' ## Summarising the results
# ' summary(sum_of_ranges) # default
# ' ## Using different options
# ' summary(sum_of_ranges, quantile = 75, cent.tend = median,
# '      rounding=  0, recall = TRUE)
# ' 
# ' @seealso \code{\link{dispRity}}
# '
# ' @author Thomas Guillerme


test.dispRity<-function(data, test, format, ...) {
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #Check if it's a bootstrapped dispRity object
    if(class(data) == "dispRity" & length(data) == 4) stop(paste(data$call), "\nUse the dispRity function to calculate disparity.", sep="")
    #must have 5 elements
    check.length(data, 5, " must be a 'dispRity' object.")
    #must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a dispRity object.")
    OBSresults<-data$disparity$observed
    #is the data bootstrapped? 
    if(!is.na(match("bootstraps", names(data$data)))) {
        #must have more than one bootstrap!
        if(length(data$data$bootstraps[[1]][[1]]) > 1) {
            is.bootstrapped<-TRUE
            BSresults<-data$disparity$bootstrapped
        } else {
            is.bootstrapped<-FALSE
        }
    } else {
        is.bootstrapped<-FALSE
    }
    
    #quantile
    #Only check if the data is bootstrapped
    if(is.bootstrapped == TRUE) {
        check.class(quantile, "numeric", " must be any value between 1 and 100.")
        #remove warnings
        options(warn=-1)
        if(any(quantile) < 1) {
            stop("Quantile(s) must be any value between 1 and 100.")
        }
        if(any(quantile) > 100) {
            stop("Quantile(s) must be any value between 1 and 100.")
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

    if(is.bootstrapped == FALSE) {
        #Extracting the observed disparity
        OBSresults_unl<-unlist(unlist(recursive.unlist(OBSresults), recursive=FALSE))

        #Create the result table
        results_table<-data.frame(cbind(rep(data$series, unlist(lapply(OBSresults_unl, length))), diversity.count(data$data$observed), as.numeric(OBSresults_unl)), stringsAsFactors=FALSE)

        #Add columns names
        colnames(results_table)<-c("series", "n", "observed")
    
    } else {
        #Extracting the observed disparity
        OBSresults_unl<-unlist(unlist(recursive.unlist(OBSresults), recursive=FALSE))

        #Unlisting the bootstrap values
        BSresults_unl<-unlist(recursive.unlist(BSresults), recursive=FALSE)
        
        #Calculating the central tendency
        results_cent<-unlist(lapply(BSresults_unl, cent.tend))

        #Multiplier (for rep)
        multiplier<-unlist(lapply(BSresults, length))

        #Create the result table
        results_table<-data.frame(cbind(rep(data$series, multiplier), diversity.count(data$data$bootstraps), rep(OBSresults_unl, multiplier), results_cent), stringsAsFactors=FALSE)

        #Add columns names
        if(is.null(match_call$cent.tend)) {
            colnames(results_table)<-c("series", "n", "observed", "mean")
        } else {
            colnames(results_table)<-c("series", "n", "observed", match_call$cent.tend)
        }

        #Checking if the observed values match the n_obs (otherwise replace by NA)
        n_obs<-diversity.count(data$data$observed)
        for(ser in 1:length(data$series)) {
            to_remove<-which(as.numeric(results_table[which(results_table$series == data$series[ser]), 2]) != n_obs[ser])
            results_table[which(results_table$series == data$series[ser]), 3][to_remove] <- NA          
        }

        #Calculate the quantiles
        results_quantile<-lapply(BSresults_unl, quantile, probs=CI.converter(quantile))

        #Add to the result table
        results_table<-cbind(results_table, matrix(data=unlist(results_quantile), ncol=length(quantile)*2, byrow=TRUE))

        #Add the quantile names
        colnames(results_table)[c(5:(length(quantile)*2+4))]<-names(results_quantile[[1]])
    }

    #Round the results (number of decimals = maximum number of digits in the entire)
    if(rounding == "default") {
        for(column in 3:ncol(results_table)) {
            if(class(results_table[,column]) != "factor") {
                results_table[,column]<-round(as.numeric(results_table[,column]), digit=get.digit(as.numeric(results_table[,column])))
            } else {
                results_table[,column]<-round(as.numeric(as.character(results_table[,column])), digit=get.digit(as.numeric(as.character(results_table[,column]))))
            }
        }
    } else {
        for(column in 3:ncol(results_table)) {
            if(class(results_table[,column]) != "factor") {
                results_table[,column]<-round(as.numeric(results_table[,column]), digit=rounding)
            } else {
                results_table[,column]<-round(as.numeric(as.character(results_table[,column])), digit=rounding)
            }
        }
    }

    #Make the rarefaction column numeric
    results_table[,2]<-as.numeric(results_table[,2])

    #----------------------
    # OUTPUT
    #----------------------
    if(recall==TRUE) {
        cat(data$call, sep="\n")
    }
    return(results_table)

}
