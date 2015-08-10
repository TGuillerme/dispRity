##########################
#Disparity functions
##########################
#Calculate the disparity as the distance from centroid
#v0.5
##########################
#SYNTAX :
#<distance> the distance matrix
#<method> the method for calculating the disparity. Can be any of the following: "volume", "centroid", "sum.range", "product.range", "sum.variance", "product.variance"
#<CI> confidence intervals (default=c(50,95))
#<bootstraps> the number of boostrap replicates (default=1000).
#<central_tendency> any function for calculating the central tendency
#<verbose> whether to be verbose or not
#<rm.last.axis> whether to remove the last axis from the pco data. Can be a threshold value.
#<save.all> whether to save all the disparity values (TRUE) or just the quantiles (FALSE (default)).
#<centroid.type> the type of centroid calculation. Can be either "median", "mean" or "full" to respectively report the median of the distances to centroid, the mean of the distances to centroid or just the distances to centroid. If null, the default will be median.
#<boot.method> bootstrapping method: either "full"(default) for resampling all the taxa per bootstrap or "single" to replace only one taxa per bootstrap.
##########################
#----
#guillert(at)tcd.ie 11/07/2015
##########################

disparity<-function(data, method=c("centroid", "sum.range", "product.range", "sum.variance", "product.variance"), CI=c(50, 95), bootstraps=1000, central_tendency=median, rarefaction=FALSE, verbose=FALSE, rm.last.axis=FALSE, save.all=FALSE, centroid.type=NULL, boot.method="full") {

    #-----------------------------
    #SANITIZING
    #-----------------------------
    #distance
    check.class(data, "matrix")

    #Test if applicable (> 2 rows)
    if(nrow(data) < 2) {
        stop("Disparity can not be calculated because less than two taxa are present in the data!")
    } 

    #method
    check.class(method, "character", " can be 'volume', 'centroid', 'sum.range', 'product.range', 'sum.variance' or/and 'product.variance'.")
    methods_list<-c('volume', "centroid", "sum.range", "product.range", "sum.variance", "product.variance")
    if(all(is.na(match(method, methods_list)))) {
        stop("method can be 'volume', 'centroid', 'sum.range', 'product.range', 'sum.variance' or/and 'product.variance'.")
    }

    #Bootstrap
    check.class(bootstraps, "numeric", " must be a single (entire) numerical value.")
    check.length(bootstraps, 1, " must be a single (entire) numerical value.")
    #Make sure the bootstrap is a whole number
    bootstraps<-round(abs(bootstraps))

    #CI
    #only relevant if bootstrap != 0)
    if(bootstraps != 0) {
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
    
    #Central tendency
    if(class(central_tendency) != "standardGeneric") {
        check.class(central_tendency, "function", " must be either a function (e.g. 'mean' or 'median'.")
    }
    
    #rarefaction
    if(class(rarefaction) != "logical") {
        check.class(rarefaction, "numeric", " must be either numeric or logical.")
    }

    #verbose
    check.class(verbose, "logical")

    #rm.last.axis
    if(class(rm.last.axis) == "logical") {
        if(rm.last.axis == FALSE) {
            rm.axis<-FALSE
        } else {
            rm.axis<-TRUE
            last.axis<-0.95
        }
    } else {
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

    #verbose
    check.class(save.all, "logical")

    #centroid
    if(any(method == "centroid")) {
        #If null, set to default
        if(is.null(centroid.type)) {
            centroid.type_function<-cen.apply.med
        } else {
            #Else, check if is right character
            check.class(centroid.type, 'character')
            check.length(centroid.type, 1, "must be a single character string.", errorif=FALSE)
            centroid.type_list<-c('median', 'mean', 'full')
            if(is.na(match(centroid.type, centroid.type_list))) {
                stop("centroid.type must be either 'median', 'mean' or 'full'.")
            }
            #Set the centroid.type function
            if(centroid.type == "median") {
                centroid.type_function<-cen.apply.med
            }
            if(centroid.type == "mean") {
                centroid.type_function<-cen.apply.mea
            }
            if(centroid.type == "full") {
                centroid.type_function<-no.apply
            }
        }
    }

    #boot.method
    check.class(boot.method, "character")
    check.length(boot.method, 1, " must be a single character string")
    boot.methods_list<-c('full', "single")
    if(all(is.na(match(boot.method, boot.methods_list)))) {
        stop("boot.method can be 'full' or 'single'.")
    }

    #-----------------------------
    #CLEANING / BOOTSTRAPING
    #-----------------------------

    #Removing the last pco axis
    if(rm.axis==TRUE) {
        #calculate the cumulative variance per axis
        scree_data<-cumsum(apply(data, 2, var) / sum(apply(data, 2, var)))
        #extract the axis  below the threshold value
        axis_selected<-length(which(scree_data < last.axis))
        #remove the extra axis
        data<-data[,c(1:axis_selected)]
        #warning
        message(paste("The", length(scree_data)-axis_selected, "last axis have been removed from the pco data."))
    }

    #Bootstraping the matrix
    #verbose
    if(verbose==TRUE) {
        message("Bootstraping...", appendLF=FALSE)
    }
    BSresult<-Bootstrap.rarefaction(data, bootstraps, rarefaction, boot.method)
    if(verbose==TRUE) {
        message("Done.", appendLF=TRUE)
    }

    #-----------------------------
    #VOLUME
    #-----------------------------
    #Hyperspace volume calculation
    if(any(method == 'volume')) {
        #Calculate the hyperspace volume
        if(verbose==TRUE) {
            message("Calculating hyperspace volume...", appendLF=FALSE)
        }
        volumes<-lapply(BSresult, volume.calc)
        #Volumes table
        Volume_table<-Disparity.measure.table(type_function=no.apply, volumes, central_tendency, CI, save.all)
        #Results type
        if(save.all == FALSE) {
            #Renaming the column
            colnames(Volume_table)[1]<-"Volume"
        } else {
            #Isolating the data parts
            Volume_values<-Volume_table[[2]]
            Volume_table<-Volume_table[[1]]
            colnames(Volume_table)[1]<-"Volume"
        }
        if(verbose==TRUE) {
            message("Done.", appendLF=TRUE)
        }
    }

    #-----------------------------
    #CENTROID
    #-----------------------------
    if(any(method == 'centroid')) {
        #Calculate the distance from centroid for the rarefaction and the bootstrapped matrices
        if(verbose==TRUE) {
            message("Calculating distance from centroid...", appendLF=FALSE)
        }
        centroids<-lapply(BSresult, centroid.calc)
        #Distance to centroid
        Centroid_dist_table<-Disparity.measure.table(type_function=centroid.type_function, centroids, central_tendency, CI, save.all)
        #Results type
        if(save.all == FALSE) {
            #Renaming the column
            colnames(Centroid_dist_table)[1]<-"Cent.dist"
        } else {
            #Isolating the data parts
            Centroid_values<-Centroid_dist_table[[2]]
            Centroid_dist_table<-Centroid_dist_table[[1]]
            colnames(Centroid_dist_table)[1]<-"Cent.dist"
        }
        if(verbose==TRUE) {
            message("Done.", appendLF=TRUE)
        }
    }

    #-----------------------------
    #RANGES
    #-----------------------------
    #Wills 1994 range calculations
    if(any(grep("range", method))) {
        #Calculate the range for the rarefaction and the bootstrapped matrices
        if(verbose==TRUE) {
            message("Calculating ranges...", appendLF=FALSE)
        }
        ranges<-lapply(BSresult, range.calc)

        #Sum of ranges
        if(any(method == 'sum.range')) {
            #Sum of range
            Sum_range_table<-Disparity.measure.table(type_function=sum.apply, ranges, central_tendency, CI, save.all)

            #Results type
            if(save.all == FALSE) {
                #Renaming the column
                colnames(Sum_range_table)[1]<-"Sum.range"
            } else {
                #Isolating the data parts
                Sum_range_values<-Sum_range_table[[2]]
                Sum_range_table<-Sum_range_table[[1]]
                colnames(Sum_range_table)[1]<-"Sum.range"
            }

        }

        #Product of ranges
        if(any(method == 'product.range')) {
            #Product of range
            Product_range_table<-Disparity.measure.table(type_function=prod.apply, ranges, central_tendency, CI, save.all)

            #Results type
            if(save.all == FALSE) {
                #Renaming the column
                colnames(Product_range_table)[1]<-"Prod.range"
            } else {
                #Isolating the data parts
                Prod_range_values<-Product_range_table[[2]]
                Product_range_table<-Product_range_table[[1]]
                colnames(Product_range_table)[1]<-"Prod.range"
            }

        }
        if(verbose==TRUE) {
            message("Done.", appendLF=TRUE)
        }
    }

    #-----------------------------
    #VARIANCE
    #-----------------------------
    #Wills 1994 variance calculations
    if(any(grep("variance", method))) {
        #Calculate the variance for the rarefaction and the bootstrapped matrices
        if(verbose==TRUE) {
            message("Calculating variance...", appendLF=FALSE)
        }
        variances<-lapply(BSresult, variance.calc)

        #Sum of variance
        if(any(method == 'sum.variance')) {
            #Sum of variance
            Sum_variance_table<-Disparity.measure.table(type_function=sum.apply, variances, central_tendency, CI, save.all)

            #Results type
            if(save.all == FALSE) {
                #Renaming the column
                colnames(Sum_variance_table)[1]<-"Sum.var"
            } else {
                #Isolating the data parts
                Sum_variance_values<-Sum_variance_table[[2]]
                Sum_variance_table<-Sum_variance_table[[1]]
                colnames(Sum_variance_table)[1]<-"Sum.var"
            }
  
        }

        #Product of variance
        if(any(method == 'product.variance')) {
            #Product of variance
            Product_variance_table<-Disparity.measure.table(type_function=prod.apply, variances, central_tendency, CI, save.all)

            #Results type
            if(save.all == FALSE) {
                #Renaming the column
                colnames(Product_variance_table)[1]<-"Prod.var"
            } else {
                #Isolating the data parts
                Prod_variance_values<-Product_variance_table[[2]]
                Product_variance_table<-Product_variance_table[[1]]
                colnames(Product_variance_table)[1]<-"Prod.var"
            }
        
        }
        if(verbose==TRUE) {
            message("Done.", appendLF=TRUE)
        }
    }

    #-----------------------------
    #OUTPUT
    #-----------------------------
    #Empty output table
    if(rarefaction==FALSE) {
        output<-matrix(nrow=1, data=rep(NA, 1))
        colnames(output)[1]<-"rarefaction"
    } else {
        output<-matrix(nrow=(nrow(data)-2), data=seq(from=3, to=nrow(data)))
        colnames(output)[1]<-"rarefaction"
    }
    #Volume
    if(any(method == 'volume')) {
        #Combine the results
        output<-cbind(output, Volume_table)
    }
    #Distance form centroid
    if(any(method == 'centroid')) {
        #Combine the results
        output<-cbind(output, Centroid_dist_table)
    }
    #Sum of ranges
    if(any(method == 'sum.range')) {
        #Combine the results
        output<-cbind(output, Sum_range_table)
    }
    #Product of ranges
    if(any(method == 'product.range')) {
        #Combine the results
        output<-cbind(output, Product_range_table)
    }
    #Sum of variance
    if(any(method == 'sum.variance')) {
        #Combine the results
        output<-cbind(output, Sum_variance_table)  
    }
    #Product of variance
    if(any(method == 'product.variance')) {
        #Combine the results
        output<-cbind(output, Product_variance_table)   
    }

    if(save.all == FALSE) {
        #Quantiles only
        return(output)
    } else {
        #Quantiles and values
        output<-list("table"=output)
        #Add the values of each metric
        #centroid
        if(any(method == 'volume')) {
            output[[length(output)+1]]<-Volume_values
            names(output)[length(output)]<-"volume"
        }
        #centroid
        if(any(method == 'centroid')) {
            output[[length(output)+1]]<-Centroid_values
            names(output)[length(output)]<-"centroid"
        }
        #Sum of sum.range
        if(any(method == 'sum.range')) {
            output[[length(output)+1]]<-Sum_range_values
            names(output)[length(output)]<-"sum.range"
        }
        #Product of ranges
        if(any(method == 'product.range')) {
            output[[length(output)+1]]<-Prod_range_values
            names(output)[length(output)]<-"product.range"
        }
        #Sum of variance
        if(any(method == 'sum.variance')) {
            output[[length(output)+1]]<-Sum_variance_values
            names(output)[length(output)]<-"sum.variance"
        }
        #Product of variance
        if(any(method == 'product.variance')) {
            output[[length(output)+1]]<-Prod_variance_values
            names(output)[length(output)]<-"product.variance"
        }

        return(output)
    }

#End
}