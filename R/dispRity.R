##########################
#time.disparity
##########################
#Calculates the disparity for interval pco.data and output a interval.disparity table object
#v0.4.3
##########################
#SYNTAX :
#<time_pco> time intervals or slices from a pco
#<relative> whether to calculate the relative disparity measurements or not.
#<...> disparity arguments (see ?disparity for information)
##########################
#----
#guillert(at)tcd.ie 20/07/2014
##########################


dispRity<-function(data, metric, boostraps=1000, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full") {

    message("dispRity:UNTESTED")
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #If matrix, transform to list
    if(class(data) == matrix) {
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

    #METRIC
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

    #BOOTSTRAP
    #Must be a numeric value
    check.class(bootstraps, "numeric", " must be a single (entire) numerical value.")
    check.length(bootstraps, 1, " must be a single (entire) numerical value.")
    #Make sure the bootstrap is a whole number
    bootstraps<-round(abs(bootstraps))

    #RAREFACTION
    if(class(rarefaction) != "logical") {
        check.class(rarefaction, "numeric", " must be either numeric or logical.")
    }

    #RM.LAST.AXIS
    #If TRUE, set automatic threshold at 0.95
    if(class(rm.last.axis) == "logical") {
        if(rm.last.axis == FALSE) {
            rm.axis<-FALSE
        } else {
            rm.axis<-TRUE
            last.axis<-0.95
        }
    } else {
        #Else must be a single numeric value (probability)
        check.class(rm.last.axis, "numeric", " must be logical or a probability threshold value.")
        check.length(rm.last.axis, 1, " must be logical or a probability threshold value.", errorif=FALSE)
        if(rm.last.axis < 0) {
            stop("rm.last.axis must be logical or a probability threshold value.")
        } else {
            if(rm.last.axis > 1) {
                stop("rm.last.axis must be logical or a probability threshold value.")
            } else {
                rm.axis<-TRUE
                last.axis<-rm.last.axis
            }
        }
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



    #BOOT.TYPE
    check.class(boot.type, "character")
    check.length(boot.type, 1, " must be a single character string")
    #Must be one of these methods
    boot.methods_list<-c('full', "single")
    if(all(is.na(match(boot.type, boot.methods_list)))) {
        stop("boot.method can be 'full' or 'single'.")
    }
    # ~~~
    # Add some extra method i.e. proportion of bootstrap shifts?
    # ~~~

    #----------------------
    #CALCULTING DISPARITY
    #----------------------

    #REMOVING THE LAST AXIS (optional)
    if(rm.axis==TRUE) {
        #Recreate the "full" matrix
        full_matrix<-data[[1]]
        for(series in 2:length(data)) {
            full_matrix<-rbind(full_matrix, data[[series]])
        }
        #Removing any duplicated taxa
        full_matrix<-full_matrix[unique(rownames(full_matrix)),]

        #calculate the cumulative variance per axis
        scree_data<-cumsum(apply(full_matrix, 2, var) / sum(apply(full_matrix, 2, var)))
        #extract the axis  below the threshold value
        axis_selected<-length(which(scree_data < last.axis))
        #remove the extra axis from the list
        data_test <- lapply(data, "[", TRUE, (1:axis_selected))
        #warning
        message(paste("The", length(scree_data)-axis_selected, "last axis have been removed from the data."))
    }

    # BOOTSRAPING THE DATA
    #verbose
    if(verbose==TRUE) message("Bootstraping...", appendLF=FALSE)
    #Bootstrap the data set 
    BSresult<-lapply(data, Bootstrap.rarefaction, bootstraps, rarefaction, boot.type)
    #verbose
    if(verbose==TRUE) message("Done.", appendLF=TRUE)

    # CALCULATING THE DISPARITY
    #verbose
    if(verbose==TRUE) message("Calculating disparity...", appendLF=FALSE)
    #Calculate disparity in all the series
    results<-lapply(BSresult, disparity.calc, class.metric, summary.metric)
    #verbose
    if(verbose==TRUE) message("Done.", appendLF=FALSE)

}



######
# Part that goes intro dispRity
#####
matrix_descriptor<-lapply(test, lapply_fun, fun=centroid.dist)
matrix_summary<-lapply(matrix_descriptor, lapply_fun, fun=mode.val)





#To utility
not.run<-TRUE
if(not.rum==FALSE) {
    #Managing bins with only one data point
    time_pco<-cor.time.pco(time_pco, minimum=3)
}


time.disparity<-function(time_pco, relative=FALSE, method=c("centroid", "sum.range", "product.range", "sum.variance", "product.variance"), CI=c(50, 95), bootstraps=1000, central_tendency=median, rarefaction=FALSE, verbose=FALSE, rm.last.axis=FALSE, save.all=FALSE, centroid.type=NULL, boot.method="full") {

    #Managing bins with only one data point
    time_pco<-cor.time.pco(time_pco, minimum=3)

    #CALCULATING THE DISPARITY FOR EACH BIN
    disparity_interval<-lapply(time_pco, disparity, method=method, CI=CI, bootstraps=bootstraps, central_tendency=central_tendency, rarefaction=rarefaction, verbose=verbose, rm.last.axis=rm.last.axis, save.all=save.all, centroid.type=centroid.type, boot.method=boot.method)



    #Return the table only
    if(save.all == FALSE) {
        #Sorting the data as a table
        if(rarefaction == FALSE) {
            #create the table's first row
            disparity_intervals_table<-disparity_interval[[1]]
            #Loop through the other elements of the table
            for(interval in 2:length(disparity_interval)) {
                disparity_intervals_table<-rbind(disparity_intervals_table, disparity_interval[[interval]])
            }
            #Renaming the rarefaction column interval
            colnames(disparity_intervals_table)[1]<-"time"
            #Saving the interval names
            disparity_intervals_table[,1]<-names(time_pco)

        } else {

            #If rarefaction has been calculated, only get the last element of each rarefaction table

            #create an interval row
            interval_row<-matrix(nrow=(nrow(disparity_interval[[1]])), data=rep(names(time_pco)[[1]]))
            #add the disparity results (with rarefaction)
            interval_tab<-cbind(interval_row, disparity_interval[[1]])
            #binding the interval table
            disparity_intervals_table<-rbind(interval_tab)

            #Loop through the other intervals
            for(interval in 2:length(time_pco)) {
                #create an interval row
                interval_row<-matrix(nrow=(nrow(disparity_interval[[interval]])), data=rep(names(time_pco)[[interval]]))
                #add the disparity results (with rarefaction)
                interval_tab<-cbind(interval_row, disparity_interval[[interval]])
                #binding the interval table
                disparity_intervals_table<-rbind(disparity_intervals_table, interval_tab)
            }

            #Renaming the rarefaction column interval
            colnames(disparity_intervals_table)[1]<-"time"
        }

        return(disparity_intervals_table)
        #print("return1")
    
    } else {

        #Sorting the data as a table
        if(rarefaction == FALSE) {
            #Creating the quantile table
            #create the table's first row
            disparity_intervals_table<-disparity_interval[[1]][[1]]
            #Loop through the other elements of the table
            for(interval in 2:length(disparity_interval)) {
                disparity_intervals_table<-rbind(disparity_intervals_table, disparity_interval[[interval]][[1]])
            }
            #Renaming the rarefaction column interval
            colnames(disparity_intervals_table)[1]<-"time"
            #Saving the interval names
            disparity_intervals_table[,1]<-names(time_pco)

        } else {

            #If rarefaction has been calculated, only get the last element of each rarefaction table

            #create an interval row
            interval_row<-matrix(nrow=(nrow(disparity_interval[[1]][[1]])), data=rep(names(time_pco)[[1]]))
            #add the disparity results (with rarefaction)
            interval_tab<-cbind(interval_row, disparity_interval[[1]][[1]])
            #binding the interval table
            disparity_intervals_table<-rbind(interval_tab)

            #Loop through the other intervals
            for(interval in 2:length(time_pco)) {
                #create an interval row
                interval_row<-matrix(nrow=(nrow(disparity_interval[[interval]][[1]])), data=rep(names(time_pco)[[interval]]))
                #add the disparity results (with rarefaction)
                interval_tab<-cbind(interval_row, disparity_interval[[interval]][[1]])
                #binding the interval table
                disparity_intervals_table<-rbind(disparity_intervals_table, interval_tab)
            }

            #Renaming the rarefaction column interval
            colnames(disparity_intervals_table)[1]<-"time"
        }

        #saving the results per time section
        disparity_intervals_values<-list()
        #Loop through all the elements of the list to extract the values
        for(interval in 1:length(disparity_interval)) {
            disparity_intervals_values[[interval]]<-disparity_interval[[interval]][[2]]
        }
        #Renaming the list elements
        names(disparity_intervals_values)<-names(time_pco)

        #output
        output<-list("quantiles"=disparity_intervals_table, "values"=disparity_intervals_values)
        return(output)
        #print("return2")
    }
}
