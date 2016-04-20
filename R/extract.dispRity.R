#' @title Extracting disparity.
#'
#' @description Extracts the disparity from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object containing disparity results.
#' @param observed A \code{logical} value indicating whether to output the observed (\code{TRUE} (default)) or the bootstrapped values (\code{FALSE}).
#' @param rarefaction Either a rarefaction value or \code{"max"} or \code{"min"} to extract the rarefaction levels (is ignored if \code{observed = TRUE}).
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat99) ; data(BeckLee_tree)
#'
#' ## Calculating some disparity
#' series <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",
#'      time = c(100, 80, 60), model = "gradual")
#' bootstraps_dat <- boot.matrix(series, bootstraps = 20, rarefaction = TRUE)
#' disparity_data <- dispRity(bootstraps_dat, metric = mean)
#'
#' ## Extracting the observed disparity
#' extract.dispRity(disparity_data)
#'
#' ## Extracting the bootstrapped disparity
#' boot_disp <- extract.dispRity(disparity_data, observed = FALSE)
#' str(boot_disp)
#' ## Or with the minimum rarefaction
#' boot_disp_rare <- extract.dispRity(disparity_data, observed = FALSE,
#'      rarefaction = "min")
#' str(boot_disp_rare)
#' ## Or a certain level of rarefaction
#' boot_disp_rare <- extract.dispRity(disparity_data, observed = FALSE,
#'      rarefaction = 5)
#' str(boot_disp_rare)
#'
#' @seealso \code{\link{dispRity}}; \code{\link{get.dispRity}}.
#'
#' @author Thomas Guillerme

#For testing
#source("sanitizing.R")
#source("extract.dispRity_fun.R")

extract.dispRity<-function(data, observed=TRUE, rarefaction) {
    #----------------------
    # SANITIZING
    #----------------------
    
    #data
    check.class(data, "dispRity")
    check.length(data, 5, " must be a 'dispRity' object output from dispRity().")

    #observed
    check.class(observed, "logical")
    if(observed == FALSE && length(data$disparity$bootstrapped) == 0) {
        stop("No bootstrapped disparity found.")
    }

    #rarefaction
    if(missing(rarefaction)) {
        rarefaction <- "max"
    }
    #checking if rare level exists
    if(observed == FALSE) {
        #If rarefaction is not max check if rarefaction exists
        if(length(grep("rarefied", data$call)) == 0 && rarefaction != "max") {
            stop("No rarefied disparity found.")
        } else {
            if(class(rarefaction) != "numeric") {
                if(rarefaction != "max") {
                    if(rarefaction != "min") {
                        stop("rarefaction must be either 'max', 'min' or a single numeric value.")
                    }
                }
            } else {
                check.class(rarefaction, "numeric"," must be either 'max', 'min' or a single numeric value.")
                check.length(rarefaction, 1," must be either 'max', 'min' or a single numeric value.")
                #does rare level exists?
                rare_check <- unique(unlist(lapply(data$data$bootstraps, lapply, lapply, nrow)))
                if(is.na(match(rarefaction, rare_check))) {
                    stop(paste("rarefaction level", rarefaction,"not found in data."))
                } else {
                    #Subtracting the "real" rarefaction level: the -2 is because the lowest level of rarefaction (1) in the data is equal to (3) taxa.
                    #Therefore, to select the rarefaction level with 3 taxa, one need to select the (3-2) element of the list.
                    rarefaction <- rarefaction - 2
                }
            }
        }
    }

    #check if is.distribution
    is.distribution <- ifelse(length(data$disparity$observed[[1]][[1]][[1]]) == 1, FALSE, TRUE)

    #----------------------
    # EXTRACTING THE DATA
    #----------------------

    if(observed == TRUE) {
        #Check if disparity is level1 (one disparity value) or level2 (one distribution)
        if(is.distribution == FALSE) {
            #Simply unlist the observed disparity
            output <- unlist(data$disparity$observed)
        } else {
            #recursively unlist the observed disparity
            output <- unlist(recursive.unlist(data$disparity$observed), recursive = FALSE)    
            names(output) <- data$series
        }
    } else {
        #Check if disparity is level1 (one disparity value) or level2 (one distribution)
        if(is.distribution == FALSE) {
            #make a list of the disparity data
            output <- unlist(lapply(recursive.unlist(data$disparity$bootstrapped), extract.rar, which.rare=rarefaction), recursive=FALSE)
            #Adding the series names
            names(output) <- data$series
        } else {
            #recursively unlist the data
            output <- unlist(lapply(recursive.unlist(data$disparity$bootstrapped, is.distribution = TRUE), extract.rar, which.rare=rarefaction), recursive = FALSE)
            #Adding the series names
            names(output) <- data$series
        }
    }

    return(output)
}

