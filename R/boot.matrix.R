boot.matrix<-function(data, bootstraps=1000, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full") {
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
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

    #BOOTSTRAP
    #Must be a numeric value
    check.class(bootstraps, "numeric", " must be a single (entire) numerical value.")
    check.length(bootstraps, 1, " must be a single (entire) numerical value.")
    #Make sure the bootstrap is a whole number
    bootstraps<-round(abs(bootstraps))

    #RAREFACTION
    if(class(rarefaction) != "logical") {
        logic.rare<-FALSE
        check.class(rarefaction, "numeric", " must be either numeric or logical.")
    } else {
        logic.rare<-TRUE
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

    #----------------------
    #BOOTSTRAPING THE DATA
    #----------------------

    #REMOVING THE LAST AXIS (optional)
    if(rm.axis==TRUE) {
        if(length(data) > 1) {
            #Recreate the "full" matrix
            full_matrix<-data[[1]]
            for(series in 2:length(data)) {
                full_matrix<-rbind(full_matrix, data[[series]])
            }
            #Removing any duplicated taxa
            full_matrix<-full_matrix[unique(rownames(full_matrix)),]
        } else {
            full_matrix<-data[[1]]
        }

        #calculate the cumulative variance per axis
        scree_data<-cumsum(apply(full_matrix, 2, var) / sum(apply(full_matrix, 2, var)))
        #extract the axis  below the threshold value
        axis_selected<-length(which(scree_data < last.axis))
        #remove the extra axis from the list
        data <- lapply(data, "[", TRUE, (1:axis_selected))
        #warning
        #message(paste("The", length(scree_data)-axis_selected, "last axis have been removed from the data."))
    }

    #BOOTSRAPING THE DATA
    #verbose
    if(verbose==TRUE) message("Bootstraping...", appendLF=FALSE)
    #Bootstrap the data set 
    BSresult<-lapply(data, Bootstrap.rarefaction, bootstraps, rarefaction, boot.type)
    #verbose
    if(verbose==TRUE) message("Done.", appendLF=TRUE)

    #Setting the output
    boot.call<-paste("Data bootstrapped ", bootstraps, " times, using the ", boot.type, " bootstrap method.", sep="")
    if(logic.rare == TRUE) {
        if(rarefaction == TRUE) {
            boot.call<-paste(boot.call, "Data was fully rarefied (down to 3 taxa).", sep="\n")
        }
    } else {
        boot.call<-paste(boot.call, "\nData was rarefied with a maximum of ", rarefaction, " taxa.", sep="")
    }
    if(rm.axis == TRUE) boot.call<-paste(boot.call, "\nThe", length(scree_data)-axis_selected, "last axis have been removed from the original data.")

    #SIZE
    taxa_list<-unlist(lapply(data, rownames))
    series_list<-names(data)
    if(is.null(series_list)) {
        series_list<-length(data)
    }

    output<-list("bootstraps"=BSresult, "taxa"=taxa_list, "series"=series_list, call=boot.call)
    class(output)<-c("dispRity", "bootstrap")

return(output)
}