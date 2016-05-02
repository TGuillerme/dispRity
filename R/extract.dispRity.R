#' @title Extracting disparity.
#'
#' @description Extracts the disparity from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object containing disparity results.
#' @param observed A \code{logical} value indicating whether to output the observed (\code{TRUE} (default)) or the bootstrapped values (\code{FALSE}).
#' @param rarefaction Either a rarefaction value or \code{"max"} or \code{"min"} to extract the rarefaction levels (is ignored if \code{observed = TRUE}).
#' @param concatenate A \code{logical} value indicating whether to concatenate the results (\code{TRUE} (default)) or to output it as a list of values (\code{FALSE}).
#' @param keep.structure A \code{logical} value indicating whether to keep the concatenation structure (\code{TRUE}) or not (\code{FALSE}, default, see details).
#' 
#' @details
#' The \code{keep.structure} allows to conserve the output structure to match the non-concatenated one (\code{concatenate = FALSE}).
#' I.e. a list of lists per series. When \code{concatenate = FALSE}, the output is \code{output[[series]][[bootstrap]] = my_disparity_values_for_that_bootstrap}.
#' When using \code{concatenate = TRUE}, the output becomes \code{output[[series]] = all_my_disparity_values_for_that_series}.
#' When using \code{keep.structure = FALSE} and \code{concatenate = TRUE}, the output becomes \code{output[[series]][[1]] = all_my_disparity_values_for_that_series} (i.e. adding a "dummy" list level).
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

extract.dispRity<-function(data, observed=TRUE, rarefaction, concatenate=TRUE, keep.structure=FALSE) {
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

    #----------------------
    # EXTRACTING THE DATA
    #----------------------

    if(observed == TRUE) {
        #Check if disparity is level1 (one disparity value) or level2 (one distribution)
        if(concatenate == TRUE) {
            #Simply unlist the observed disparity
            output <- unlist(data$disparity$observed)
        } else {
            #recursively unlist the observed disparity
            output <- unlist(recursive.unlist(data$disparity$observed), recursive = FALSE)
            names(output) <- data$series
        }
    } else {
        #Check if disparity is level1 (one disparity value) or level2 (one distribution)
        if(concatenate == TRUE) {
            #make a list of the disparity data
            output <- unlist(lapply(recursive.unlist(data$disparity$bootstrapped), extract.rar, which.rare = rarefaction), recursive = FALSE)
            if(keep.structure == TRUE) {
                #put each element per series back in a list (of one element)
                output <- lapply(output, list)
            }
            #Adding the series names
            names(output) <- data$series
        } else {
            #recursively unlist the data
            output <- unlist(lapply(recursive.unlist(data$disparity$bootstrapped, is.distribution = TRUE), extract.rar, which.rare = rarefaction), recursive = FALSE)
            #Adding the series names
            names(output) <- data$series
        }
    }

    return(output)
}

