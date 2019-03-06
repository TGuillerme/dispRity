#' @title dispRity object summary
#'
#' @description Creates a summary of a \code{dispRity} object.
#'
#' @param object A \code{dispRity} object.
#' @param ... Additional arguments to be passed to \code{\link{summary}}.
#' @param quantiles The quantiles to display (default is \code{quantiles = c(50, 95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[stats]{median}}).
#' @param recall \code{logical} value specifying whether to recall the \code{dispRity} parameters input (default = \code{FALSE}).
#' @param digits Optional, a value for digits the values in the output table (default = 2).
# ' @param results Optional, in the case of summarising a \code{\link{sequential.test}} which results to display (default = "coefficients")
#'
#' @return
#' A \code{data.frame} with:
#' \item{subsets}{the subset names.}
#' \item{n}{the number of elements in each subset.}
#' \item{observed}{the observed disparity or the the observed central tendency (<cent_tend>) of disparity (\code{obs.<cent_tend>}).}
#' \item{bootstraps...}{if \code{data} is bootstrapped, the bootstrapped disparity's central tendency (\code{bs.<cent_tend>}) and the quantiles of the bootstrapped disparities (or, if \code{data} is not bootstrapped but disparity is calculated as a distribution - see \code{\link[dispRity]{dispRity}}) - the quantiles of the observed disparity are displayed).}
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#'
#' ## Summarising the results
#' summary(disparity) # default
#' ## Using different options
#' summary(disparity, quantiles = 75, cent.tend = mean, digits = 8,
#'      recall = TRUE)
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{plot.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# source("make.metric.R")
# source("summary.dispRity_fun.R")
# data(BeckLee_mat50)
# groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12), rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_subsets <- custom.subsets(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 3, rarefaction = TRUE)
# subsets <- extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = TRUE)
# data <- sequential.test(subsets, family = gaussian, correction = "hommel")

# data <- dispRity(bootstrapped_data, metric = variances)

# quantiles <- c(50, 95)
# cent.tend <- median
# recall <- FALSE
# match_call <- list() ; match_call$cent.tend <- "median"

summary.dispRity <- function(object, ..., quantiles = c(50, 95), cent.tend = median, recall = FALSE, digits){#, results = "coefficients") {

    ## Renaming object
    data <- object

    #----------------------
    # SANITIZING
    #----------------------

    #Get call
    match_call <- match.call()
    # warning("DEBUG summary.dispRity") ; return(match_call)

    #cent.tend
    #Must be a function
    check.class(cent.tend, "function")
    #The function must work
    if(make.metric(cent.tend, silent = TRUE) != "level1") {
        stop.call(match_call$cent.tend, " cannot be used for measuring the central tendency.")
    }
    ## Update match_call if argument is empty
    if(is.null(match_call$cent.tend)) match_call$cent.tend <- "median"

    #recall
    check.class(recall, "logical")

    #digits
    if(missing(digits)) {
        #Set to default (see below)
        digits <- "default"
    } else {
        check.class(digits, "numeric")
    }

    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #Check if it is a bootstrapped dispRity object
    if(is.null(data$disparity) && is.na(class(data)[2])) {
        stop.call("", "Disparity has not been calculated yet.\nUse the dispRity() function to do so.")
    }
    
    #Check quantiles
    check.class(quantiles, "numeric", " must be any value between 1 and 100.")
    if(any(quantiles < 1) | any(quantiles > 100)) {
        stop.call("", "quantiles(s) must be any value between 1 and 100.")
    }

    #----------------------
    # SPECIAL SHORTCUTS (dual classes)
    #----------------------
    if(length(class(data)) > 1) {

        ## Model test summary
        if(class(data)[2] == "model.test") {
            ## Extracting the AICs and the log likelihoods
            base_results <- cbind(data$aic.models, "log.lik" = sapply(data$full.details, function(x) x$value))

            ## Extracting the additional parameters
            parameters <- lapply(data$full.details, function(x) x$par)

            # MP: allow summaries to work on a single model
            if(length(parameters) == 1)  {
            	param.tmp <- c(parameters[[1]])
            	#names(param.tmp) <- rownames(parameters[[1]])
            	parameters <- list(param.tmp)
            	}
            base_results <- cbind(base_results, "param" = unlist(lapply(parameters, length)))
            
            ## Get the full list of parameters
            
           	names_list <- lapply(parameters, names)
           	full_param <- unique(unlist(names_list))

            output_table <- cbind(base_results, do.call(rbind, lapply(parameters, match.parameters, full_param)))
           
            ## Rounding
            summary_results <- digits.fun(output_table, digits, model.test = TRUE)

            return(summary_results)
        }
        
        # Model sim summary
        if(class(data)[2] == "model.sim") {

            # if(recall){
            #     print.dispRity(data)
            # }

            ## Extract the central tendencies
            simulation_data_matrix <- sapply(data$simulation.data$sim, function(x) x$central_tendency)

            ## Get the quantiles
            simulation_results <- apply(simulation_data_matrix, 1, get.summary, cent.tend = cent.tend, quantiles = quantiles)
            simulation_results <- cbind(do.call(rbind, lapply(simulation_results, function(X) rbind(X$cent_tend[[1]]))),
                                        do.call(rbind, lapply(simulation_results, function(X) rbind(X$quantiles))))
            colnames(simulation_results)[1] <- as.character(match_call$cent.tend)

            ## Output table
            output_table <- cbind("subsets" = rev(data$simulation.data$fix$subsets),
                                  "n" = data$simulation.data$fix$sample_size,
                                  "var" = unname(data$simulation.data$fix$variance),
                                  simulation_results)
            rownames(output_table) <- seq(1:nrow(output_table))
            return(output_table)
        }

        ## No dual class summary available
        stop.call("", paste0("No specific summary for combined class \"dispRity\" and \"", class(data)[2], "\"."))
    } 

    #----------------------
    # TRANSFORMING THE DATA INTO A TABLE
    #----------------------

    ## Check if disparity is a value or a distribution
    is_distribution <- ifelse(length(data$disparity[[1]]$elements) != 1, TRUE, FALSE)

    ## Check the bootstraps
    bootstrapped <- ifelse(!is.null(data$call$bootstrap), TRUE, FALSE)

    ## Get the elements per subsets
    elements <- lapply(data$subsets, lapply.get.elements, bootstrapped)
    nulls <- unlist(lapply(elements, is.null))
    if(any(nulls)) {
        for(null_elem in which(nulls)) {
            elements[[null_elem]] <- nrow(data$subsets[[null_elem]]$elements)
        }
    }
    
    ## Get the names of the subsets
    names <- names(data$subsets)
    if(is.null(names)) {
        names <- seq(1:length(data$subsets))
    }

    ## Get the disparity values
    disparity_values <- lapply(data$disparity, lapply.observed)
    names(disparity_values) <- NULL

    ## Initialise the results
    summary_results <- data.frame(row.names = NULL, "subsets" = rep(names, unlist(lapply(elements, length))), "n" = unlist(elements))

    ## Add the observed values
    if(is_distribution) {
        summary_results <- cbind(summary_results, as.vector(unlist(mapply(mapply.observed, lapply(disparity_values, cent.tend), elements))), row.names = NULL)
        names(summary_results)[3] <- paste("obs", as.expression(match_call$cent.tend), sep = ".")
    } else {
        summary_results <- cbind(summary_results, as.vector(unlist(mapply(mapply.observed, disparity_values, elements))), row.names = NULL)
        names(summary_results)[3] <- "obs"
    }

    if(!is.null(data$call$bootstrap)) {
        ## Calculate the central tendencies and the quantiles
        summary_results <- cbind(summary_results, matrix(unlist(lapply(data$disparity, lapply.summary, cent.tend, quantiles)), byrow = TRUE, ncol = (1+length(quantiles)*2)))
        ## Adding the labels
        names(summary_results)[4:length(summary_results)] <- c(paste("bs", as.expression(match_call$cent.tend), sep = "."), names(quantile(rnorm(5), probs = CI.converter(quantiles))))
    } else {
        if(is_distribution) {
            ## Calculate the quantiles
            summary_results <- cbind(summary_results, matrix(unlist(lapply(data$disparity, lapply, get.summary, quantiles = quantiles)), byrow = TRUE, ncol = (length(quantiles)*2)))
            ## Adding the labels
            names(summary_results)[4:length(summary_results)] <- c(names(quantile(rnorm(5), probs = CI.converter(quantiles))))
        }
    }

    ## Round the results (number of decimals = maximum number of digits in the output)
    summary_results <- digits.fun(summary_results, digits)

    ## If any elements is equal to one, check if not NA
    if(any(summary_results$n == 1)) {
        ## Select the values to check
        to_check <- which(summary_results$n == 1)
        ## Get their replacement values

        check.elements.NA <- function(row, summary_results, data) {
            ## Check if the subset contains NA elements
            ifelse(is.na(data$subsets[[as.character(summary_results[row, 1])]]$elements[1,1]), 0, 1)
        }

        replace_vals <- sapply(to_check, check.elements.NA, summary_results, data)
        ## Replace them in the results
        summary_results[to_check, 2] <- replace_vals
    }

    #----------------------
    # OUTPUT
    #----------------------
    if(recall) print.dispRity(data)

    return(summary_results)

    #Summary sequential.test shortcut
    # if(length(class(data)) == 2) {
    #     if(class(data)[[1]] == "dispRity" && class(data)[[2]] == "seq.test") {
    #         #Results sanitizing
    #         check.class(results, "character") 
    #         #At least one must be "coefficients"
    #         if(is.na(match("coefficients", results))) {
    #             stop("At least one of the returned results must be 'coefficients'.")
    #         }
    #         #Results must be at least coefficients
    #         if(is.na(match("coefficients", results))) {
    #             results <- c(results, "coefficients")
    #         }

    #         #Creating the table results
    #         results_out <- summary.seq.test(data, quantiles, cent.tend, recall, digits, results, match_call)

    #         #Checking if distribution
    #         is.distribution <- ifelse(length(data$models[[1]]) == 1, FALSE, TRUE)

    #         #Rounding the results
    #         if(is.distribution == FALSE) {
    #             results_out <- lapply(results_out, digits.fun, digits, seq.test = TRUE)
    #         } else {
    #             results_out <- lapply(results_out, lapply, digits.fun, digits, seq.test = TRUE)
    #         }

    #         return(results_out)
    #     }
    #     if(class(data)[1] == "dispRity" & class(data)[2] == "randtest") {
    #         #No summary
    #         return(data)
    #     }
    # }


}
