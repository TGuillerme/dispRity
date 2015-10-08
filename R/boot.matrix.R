#' @title Bootstraps an rarefies ordinated data.
#'
#' @description Bootstraps and rarefies either a single ordinated matrix or a list of ordinated matrices.
#'
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)} or a list of matrices (typically output from \link{time.series} or \link{cust.series}).
#' @param bootstraps The number of bootstrap pseudo-replicates (\code{default = 1000}).
#' @param rarefaction Either a \code{logical} value whether to fully rarefy the data or a set of \code{numeric} values to rarefy the data.
#' @param rm.last.axis Either a \code{logical} value whether to remove the last axis of the ordinated matrix or a proportion of axis to save.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' @param boot.type The bootstrap algorithm to use (\code{default = "full"}; see details).
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the observed and boostraped matrices.}
#' \item{elements}{A \code{vector} containing all the names of the elements from the original matrix.}
#' \item{series}{A \code{vector} containing the name of the series (is \code{"1"} if the input was a single \code{matrix}).}
#' \item{call}{A \code{vector} containing the arguments used for the bootstraping.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#'
#' @details  
#' \code{rarefaction}: when the input is \code{numeric}, the number of elements is set to the value(s) for each bootstrap.
#'  
#' \code{rm.last.axis}: the provided \code{numeric} value should be the percentage of axis to keep. By default when \code{rm.last.axis = TRUE}, 95% of the axis are preserved (the last 5% are removed).
#'
#' \code{boot.type}: the different bootstrap algorithms are:
#' \itemize{
#'   \item \code{"full"}: resamples all the rows of the matrix and replaces them with a new random sample of rows (with \code{replace = TRUE}, meaning all the elements can be duplicated in each bootstrap).
#'   \item \code{"single"}: resamples only one row of the matrix and replaces it with a new radnomly sampled row (with \code{replace = FALSE}, meaning that only one elements can be duplicated in each boostrap).
#' }
#'
#' @examples
#' ## Load the Beck & Lee 2014 matrix
#' data(BeckLee_mat50)
#' 
#' ## Bootstrapping a matrix
#' ## Bootstrapping an ordinated matrix 20 times
#' boot.matrix(BeckLee_mat50, bootstraps = 20)
#' ## Bootstrapping an ordinated matrix with rarefaction
#' boot.matrix(BeckLee_mat50, bootstraps = 20, rarefaction = TRUE)
#' ## Bootstrapping an ordinated matrix with only 7,10 and 11 elements sampled
#' boot.matrix(BeckLee_mat50, bootstraps = 20, rarefaction = c(7,10,11))
#' ## Bootstrapping an ordinated matrix with only 90% of the first axis
#' boot.matrix(BeckLee_mat50, bootstraps = 20, rm.last.axis = 0.9)
#' 
#' ## Bootstrapping a series of matrices
#' ## Generating a dummy series of matrices
#' ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
#' factors <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))
#' matrix.list <- cust.series(ordinated_matrix, factors)
#' ## Bootstrapping the series of matrices 20 times (each)
#' boot.matrix(matrix.list, bootstraps = 20)
#' 
#' @author Thomas Guillerme

boot.matrix<-function(data, bootstraps=1000, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full") {
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #If class is dispRity, data is serial
    if(class(data) == "dispRity") {
        #Must be proper format
        check.length(data, 3, " must be either a matrix, a list of matrices or an output from time.series or cust.series.")
        #Extracting the info
        taxa_list<-data$elements
        series_list<-data$series[-1]
        series_type<-data$series[1]
        data<-data$data
        #Set previous call logic
        prev_call<-TRUE

    } else {
        #Set previous call logic
        prev_call<-FALSE

        #If data is matrix, transform to list
        if(class(data) == "matrix") {
            data<-list(data)
        }

        #Data must be a list
        check.class(data, "list", " must be either a matrix, a list of matrices or an output from time.series or cust.series.")

        #Extract info
        taxa_list<-unlist(lapply(data, rownames))
        names(taxa_list)<-NULL
        series_list<-names(data)
        if(is.null(series_list)) {
            series_list<-length(data)
        }
    }

    #Each matrix must have the same number of columns
    mat_columns<-unique(unlist(lapply(data, ncol)))
    if(length(mat_columns) != 1) stop("Some matrices in data have different number of columns.")

    #Making sure there is at least 3 rows per element
    if(any(unlist(lapply(data, nrow) < 3))) stop("Some matrices in data have less than 3 rows.")

    #BOOTSTRAP
    #Must be a numeric value
    check.class(bootstraps, "numeric", " must be a single (entire) numerical value.")
    check.length(bootstraps, 1, " must be a single (entire) numerical value.")
    #Make sure the bootstrap is a whole number
    bootstraps<-round(abs(bootstraps))

    #RAREFACTION
    #Is it not logical?
    if(class(rarefaction) != "logical") {
        logic.rare<-FALSE
        #Is it numeric?
        check.class(rarefaction, "numeric", " must be either numeric or logical.")
        #Is it only one value?
        if(length(rarefaction) == 1) {
            rare.list<-FALSE
        } else {
            rare.list<-TRUE
        }
    } else {
        logic.rare<-TRUE
        rare.list<-FALSE
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
        stop("boot.method must be 'full' or 'single'.")
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
        #Else must be a single numeric value (proportional)
        check.class(rm.last.axis, "numeric", " must be logical or a proportional threshold value.")
        check.length(rm.last.axis, 1, " must be logical or a proportional threshold value.", errorif=FALSE)
        if(rm.last.axis < 0) {
            stop("rm.last.axis must be logical or a proportional threshold value.")
        } else {
            if(rm.last.axis > 1) {
                stop("rm.last.axis must be logical or a v threshold value.")
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
    #Getting the observed results
    OBSresult<-lapply(data, Bootstrap.rarefaction, bootstraps=0, rarefaction=FALSE, boot.type="full")
    #verbose
    if(verbose==TRUE) message("Done.", appendLF=TRUE)

    #Setting the output
    boot.call<-paste("Data was bootstrapped ", bootstraps, " times, using the ", boot.type, " bootstrap method.", sep="")

    #Series
    if(prev_call == TRUE) {
        boot.call<-paste("Data was split using ", series_type, " method.\n", boot.call, sep="")
    }

    #Rarefaction
    if(logic.rare == TRUE) {
        if(rarefaction == TRUE) {
            boot.call<-paste(boot.call, "Data was fully rarefied (down to 3 elements).", sep="\n")
        }
    } else {
        if(rare.list == FALSE) {
            boot.call<-paste(boot.call, "\nData was rarefied with a maximum of ", rarefaction, " elements", sep="")
        } else {
            rare.elements<-paste(paste(rarefaction[-length(rarefaction)], collapse=", "), rarefaction[length(rarefaction)], sep=" and ")
            boot.call<-paste(boot.call, "\nData was rarefied with a maximum of ", rare.elements, " elements", sep="")
        }
    }

    #Remove last axis
    if(rm.axis == TRUE) boot.call<-paste(boot.call, "\nThe", length(scree_data)-axis_selected, "last axis were removed from the original ordinated data.")

    #SIZE
    output<-list("data"=list("bootstraps"=BSresult, "observed"=OBSresult), "elements"=taxa_list, "series"=series_list, "call"=boot.call)
    class(output)<-c("dispRity")

return(output)
}