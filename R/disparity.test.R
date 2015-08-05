##########################
#Disparity testing function
##########################
#Calculates the differences between PCO intervals based on Anderson & Friedman 2012 test.
#v0.0.3
##########################
#SYNTAX :
#<time_pco> a time_pco matrix
#<method> the method for calculating the disparity. Can be any of the following: "volume", "centroid", "sum.range", "product.range", "sum.variance", "product.variance"
#<test> the type of test to run "pairwise" (every interval to each other), "sequential" (consecutive intervals only) or "reference" (the first interval to all the others).
#<bootstraps> the number of bootstrap replicates (default=1000).
#<correction> one of the methods from p.adjust function to correct the p-values. If "none", no correction will be applied.
#<rarefaction> a rarefaction value. If NULL, is ignored.
#<rm.last.axis> whether to remove the last axis from the pco time_pco. Can be a threshold value.
##########################
#----
#guillert(at)tcd.ie 11/07/2015
##########################

disparity.test<-function(time_pco, method, test, bootstraps=1000, correction="bonferroni", rarefaction=NULL, rm.last.axis=FALSE, boot.method="full") { #verbose=FALSE
    #-----------------------------
    #SANITIZING
    #-----------------------------
    #time_pco
    check.class(time_pco, "list")
    check.class(time_pco[[1]], "matrix")
    if(length(time_pco) < 2) {
        stop("time_pco must have a least to intervals.")
    }

    #method
    check.class(method, "character", " can be 'volume', 'centroid', 'sum.range', 'product.range', 'sum.variance' or/and 'product.variance'.")
    methods_list<-c('volume', "centroid", "sum.range", "product.range", "sum.variance", "product.variance")
    check.length(method, 1, ": only one method can be input")
    if(all(is.na(match(method, methods_list)))) {
        stop("method must be 'volume', 'centroid', 'sum.range', 'product.range', 'sum.variance' or/and 'product.variance'.")
    }

    #test
    check.class(test, "character", " can be 'pairwise', 'sequential' or 'reference'.")
    test_list<-c('pairwise', "sequential", "reference")
    check.length(test, 1, ": only one method can be input")
    if(all(is.na(match(test, test_list)))) {
        stop("test must be 'pairwise', 'sequential' or 'reference'.")
    }    

    #Bootstrap
    check.class(bootstraps, "numeric", " must be a single (entire) numerical value.")
    check.length(bootstraps, 1, " must be a single (entire) numerical value.")
    #Make sure the bootstrap is a whole number
    bootstraps<-round(abs(bootstraps))
    
    #correction
    check.class(correction, 'character')
    p.adjust_list<- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
    if(all(is.na(match(correction, p.adjust_list)))) {
        stop("correction type must be one of the p.adjust function options.")
    }
    if(length(time_pco) > 2 & correction == "none") {
        message("Multiple p-values will be calculated without adjustment!\nThis will inflate the probability of having significant results.")
    }
    
    #rarefaction
    if(is.null(rarefaction)) {
        rarefaction<-FALSE
    } else {
        check.class(rarefaction, "numeric")
    }

    #verbose
    #check.class(verbose, "logical") #deactivated for now

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

    #boot.method
    check.class(boot.method, "character")
    check.length(boot.method, 1, " must be a single character string")
    boot.methods_list<-c('full', "single")
    if(all(is.na(match(boot.method, boot.methods_list)))) {
        stop("boot.method can be 'full' or 'single'.")
    }

    #centroid.type - forcing mean
    #centroid.type_function<-cen.apply.mea

    #if(!exists("centroid.type")) {
    #    centroid.type_function<-cen.apply.mea
    #} else {
        #Else, check if is right character
    #    check.class(centroid.type, 'character')
    #    check.length(centroid.type, 1, "must be a single character string.", errorif=FALSE) 
    #    centroid.type_list<-c('median', 'mean')
    #    if(is.na(match(centroid.type, centroid.type_list))) {
    #        stop("centroid.type must be either 'median' or 'mean' ('full' cannot be used).")
    #    } 
        #Set the centroid.type function
    #    if(centroid.type == "median") {
    #        centroid.type_function<-cen.apply.med
    #    }
    #    if(centroid.type == "mean") {
    #        centroid.type_function<-cen.apply.mea
    #    }
    #}

    #-----------------------------
    #CLEANING / BOOTSTRAPING
    #-----------------------------

    #Removing the last pco axis
    if(rm.axis==TRUE) {
        #calculate the cumulative variance per axis
        scree_data<-cumsum(apply(time_pco, 2, var) / sum(apply(time_pco, 2, var)))
        #extract the axis  below the threshold value
        axis_selected<-length(which(scree_data < last.axis))
        #remove the extra axis
        time_pco<-time_pco[,c(1:axis_selected)]
        #warning
        message(paste("The", length(scree_data)-axis_selected, "last axis have been removed from the pco time_pco."))
    }

    #Bootstraping the matrix
    #verbose
    #if(verbose==TRUE) {
    #    message("Bootstraping...", appendLF=FALSE)
    #}

    BSresult<-lapply(time_pco, Bootstrap.rarefaction, bootstraps, rarefaction, boot.method="full")
    #if(verbose==TRUE) {
    #    message("Done.", appendLF=TRUE)
    #}

    #-----------------------------
    #SETTING THE FUNCTIONS (according to metric)
    #-----------------------------

    #Volume
    if(method == 'volume') {
        stop("'volume' not implemented yet.")
        method.fun<-volume.calc
        apply.fun<-NULL
    }

    #Centroid
    if(method == 'centroid') {
        method.fun<-centroid.calc
        apply.fun<-mean
    }

    #Ranges
    if(method == 'sum.range') {
        method.fun<-range.calc
        apply.fun<-sum
    }
    if(method == 'product.range') {
        method.fun<-range.calc
        apply.fun<-prod
    }

    #Variance
    if(method == 'sum.variance') {
        method.fun<-variance.calc
        apply.fun<-sum
    }
    if(method == 'product.variance') {
        method.fun<-variance.calc
        apply.fun<-prod
    }

    #-----------------------------
    #RUNNING THE TEST
    #-----------------------------

    #ERROR IN CALCULATING THE DISPARITY! Problem with apply.fun or method.fun?

    #Calculating the metric table (BS values * intervals)
    BSresults<-matrix(NA, nrow=bootstraps, ncol=length(time_pco))
    for (int in 1:length(time_pco)) {
        BSresults[,int]<-apply(lapply(BSresult[[int]],method.fun)[[1]], 1, apply.fun)
    }

    #Running the pairwise test
    test_results<-Anderson.test(BSresults, time_pco)

    #-----------------------------
    #OUTPUT
    #-----------------------------

    #Creating the output table (depending on test)
    if(test == "pairwise") {
        #Number of comparisons
        n_comparisons<-length(time_pco)*(length(time_pco)-1)/2

        #Generating the row names
        row_names<-vector()
        for (row in 1:(length(time_pco)-1)) {
            for (col in (row+1):length(time_pco)) {
                row_names<-c(row_names, paste(rownames(test_results[[1]])[row], colnames(test_results[[1]])[col], sep=":"))
            }
        }

        #Making the output table
        output_table<-as.data.frame(matrix(NA, nrow=n_comparisons, ncol=5))
        colnames(output_table)<-c("difference", "Df", "T", "p.value", " ")
        rownames(output_table)<-row_names
        
        if(any(unlist(lapply(test_results, is.na)))) {
            #Output an NA table
            no_test<-TRUE
        } else {
            #Build the output table
            rounds<-c(2,0,3,5)
            for (col in 1:(ncol(output_table)-1)) {
                output_table[,col]<-round(test_results[[col]][lower.tri(test_results[[col]])], rounds[col])
            }
            output_table[, 5]<-rep(" ", n_comparisons)

            #Applying the p-value correction
            output_table$p.value<-round(p.adjust(test_results$p[lower.tri(test_results$p)], method=correction),5)

            #Adding significant tokens
            output_table[,5]<-signif.token(output_table$p.value)
        }
    
    } else {

        #Test is sequential
        if(test == "sequential") {
            #Number of comparisons
            n_comparisons<-length(time_pco)-1

            #Generating the row names
            row_names<-vector()
            for (row in 1:n_comparisons) {
                row_names<-c(row_names, paste(rownames(test_results[[1]])[row], rownames(test_results[[1]])[row+1], sep=":"))
            }
            
            #Matrix sequence
            row_seq<-c(seq(from=1, to=n_comparisons))
            col_seq<-c(seq(from=2, to=length(time_pco)))
        
        } else {
        #Test is reference
            #Number of comparisons
            n_comparisons<-length(time_pco)-1

            #Generating the row names
            row_names<-vector()
            for (row in 1:n_comparisons) {
                row_names<-c(row_names, paste(rownames(test_results[[1]])[1], rownames(test_results[[1]])[row+1], sep=":"))
            }

            #Matrix sequence
            row_seq<-c(rep(1, n_comparisons))
            col_seq<-c(seq(from=2, to=length(time_pco)))
        }

        #Making the output table
        output_table<-as.data.frame(matrix(NA, nrow=n_comparisons, ncol=5))
        colnames(output_table)<-c("difference", "Df", "T", "p.value", " ")
        rownames(output_table)<-row_names

        if(any(unlist(lapply(test_results, is.na)))) {
            #Output an NA table
            no_test<-TRUE
        } else {
            no_test<-FALSE
            #Build the output table
            rounds<-c(2,0,3,5)

            for(col in 1:(ncol(output_table)-1)) {
                for (seq in 1:n_comparisons) {
                    output_table[seq,col]<-round(test_results[[col]][row_seq[seq],col_seq[seq]], rounds[col])
                }
            }
            output_table[, 5]<-rep(" ", n_comparisons)

            #Applying the p-value correction
            pvals<-vector()
            for (seq in 1:n_comparisons) {
                pvals<-c(pvals, test_results$p[row_seq[seq],col_seq[seq]])
            }
            output_table$p.value<-round(p.adjust(pvals, method=correction),5)

            #Adding significant tokens
            output_table[,5]<-signif.token(output_table$p.value)
        }
    }

    #reporting details
    signif.codes<-"Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
    test<-paste(simpleCap(test), " differences test between the ", method,".", sep="")
    boots<-paste("Data set was bootstrapped ", bootstraps, " times.", sep="")
    corr<-paste(simpleCap(correction), " correction applied was applied to p-values.", sep="")

    #Output
    if(no_test==TRUE){
        return(list("results"=output_table))
    } else {
        if(rarefaction != FALSE) {
            rare<-paste("Differences are calculated on the rarefied data-set (", rarefaction, " taxa per interval).", sep="")
            return(list("results"=output_table, "details"=c(signif.codes, test, boots, corr, rare)))
        } else {
            return(list("results"=output_table, "details"=c(signif.codes, test, boots, corr)))
        }
    }
#End
}