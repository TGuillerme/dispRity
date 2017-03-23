## Converts one or more CI into a quantile probabilities
CI.converter <- function(CI) {
    sort(c(50-CI/2, 50+CI/2)/100)
}

# Wrapping function for summarising a single rarefaction
get.summary <- function(disparity_subsamples_rare, cent.tend, quantiles) {
    output <- list()
    # output$cent_tend <- apply(disparity_subsamples_rare, 1, cent.tend)
    if(!missing(cent.tend)) {
        output$cent_tend <- cent.tend(as.vector(disparity_subsamples_rare))
    }
    # output$quantiles <- apply(disparity_subsamples_rare, 1, quantile, probs = CI.converter(quantiles))
    if(!missing(quantiles)) {
        output$quantiles <- quantile(as.vector(disparity_subsamples_rare), probs = CI.converter(quantiles))
    }
    return(output)
}

## Lapply wrapping function for summarising a single subsamples
lapply.summary <- function(disparity_subsamples, cent.tend, quantiles) {
    return(lapply(disparity_subsamples[-1], get.summary, cent.tend, quantiles))
}

## Lapply wrapper for getting elements
lapply.get.elements <- function(subsamples, bootstrapped = TRUE) {
    if(bootstrapped){
        return(unlist(lapply(subsamples[-1], nrow)))
    } else {
        return(unlist(lapply(subsamples, nrow)))
    }
}

## Lapply wrapper for getting the disparity observed values
lapply.observed <- function(disparity) {
    return(c(disparity$elements))
}

## Mapply wrapper for getting the disparity observed values
mapply.observed <- function(disparity, elements) {
    return(c(disparity, rep(NA, (length(elements)-1))))
}

## Get digit for table
get.digit <- function(column) {
    if(max(nchar(round(column)), na.rm = TRUE) <= 4) {
        return(4-max(nchar(round(column)), na.rm = TRUE))
    } else {
        return(0)
    }
}

## Function for rounding the results
rounding.fun <- function(results_table, rounding, seq.test = FALSE) {

    ## seq.test
    if(seq.test != FALSE) {
        start_column <- 1
    } else {
        start_column <- 3
    }

    if(rounding != "default") {
        for(column in start_column:ncol(results_table)) {
            if(class(results_table[,column]) != "factor") {
                results_table[,column] <- round(as.numeric(results_table[,column]), digits = rounding)
            } else {
                results_table[,column] <- round(as.numeric(as.character(results_table[,column])), digits = rounding)
            }
        }

    } else {
        for(column in start_column:ncol(results_table)) {
            if(class(results_table[,column]) != "factor") {
                if(any(!is.na(results_table[,column]))) {
                    results_table[,column] <- round(as.numeric(results_table[,column]), digits = get.digit(as.numeric(results_table[,column])))
                }
            } else {
                if(any(!is.na(results_table[,column]))) {
                    results_table[,column] <- round(as.numeric(as.character(results_table[,column])), digits = get.digit(as.numeric(as.character(results_table[,column]))))
                }
            }
        }
    }
    return(results_table)
}











# ## Saving results function
# save.results.seq.test <- function(model, results) {
#     save_out <- match(results, names(summary(model)))
#     return(summary(model)[save_out])
# }

# ## Transform a matrix (usually the coefficient results) into a list
# matrix.to.list <- function(matrix, no.intercept = TRUE) {
#     ## Remove the intercept column (if needed)
#     if(no.intercept != FALSE) {
#         if(any(rownames(matrix) == "(Intercept)")) {
#             matrix <- matrix[-which(rownames(matrix) == "(Intercept)"),]
#         }
#     }

#     ## Transform the matrix into a list
#     output <- as.list(matrix)
#     ## Adding names (if necessary)
#     if(is.null(names(output))) {
#         names(output) <- colnames(matrix)
#     }
#     return(output)
# }

# ## Relists a list (recursive) with element names
# relist.names <- function(element, elements_names) {
#     output <- list(element)
#     names(output) = elements_names
#     return(output)
# }

# ## Gets results table from results elements
# get.results.table <- function(element, results_elements, cent.tend, quantiles, comparisons, match_call, is.distribution) {
#     ## Get the data
#     if(is.null(element)) {
#         element = 1
#     }
#     data <- lapply(lapply(results_elements, lapply, `[[`, element), unlist)
    
#     ## Central tendency
#     results_table <- matrix(data = lapply(data, cent.tend), ncol = 1, dimnames = list(comparisons))
#     if(!is.null(match_call$cent.tend)) {
#         colnames(results_table) <- paste(match_call$cent.tend)
#     } else {
#         colnames(results_table) <- "mean"
#     }

#     if(is.distribution != FALSE) {
#         ## Quantiles
#         results_quantiles <- lapply(data, quantile, probs = CI.converter(quantiles))
#         ## Create table
#         results_table <- cbind(results_table, matrix(data = unlist(results_quantiles), nrow = length(comparisons), byrow = TRUE, dimnames = list(c(), c(names(results_quantiles[[1]])))))
#     }
#     return(results_table)
# }

# summary.seq.test <- function(data, quantiles, cent.tend, recall, rounding, results, match_call) {
    
#     ## SANITIZING
#     ## quantiles
#     check.class(quantiles, "numeric", " must be any value between 1 and 100.")
#     ## remove warnings
#     if(any(quantiles < 1)) {
#         stop("quantiles(s) must be any value between 1 and 100.")
#     }
#     if(any(quantiles > 100)) {
#         stop("quantiles(s) must be any value between 1 and 100.")
#     }

#     ## Check if is distribution
#     is.distribution <- ifelse(length(data$models[[1]]) == 1, FALSE, TRUE)

#     ## SAVING THE RESULTS
#     models_results <- lapply(data$models, lapply, save.results.seq.test, results)
#     intercepts_results <- lapply(data$intercepts, unlist)
#     comparisons <- unique(unlist(names(data$models)))

#     ## Getting the slopes
#     if(is.distribution != FALSE) {
#         ## Getting the coefficients
#         ## Transforming the coefficients into a list
#         results_coefficients <- lapply(lapply(models_results, lapply, `[[`, 1), lapply, matrix.to.list, no.intercept = TRUE)

#         ## Creating the tables for each element in the matrices
#         elements_list_matrix <- as.list(names(results_coefficients[[1]][[1]]))
#         table_matrix <- lapply(elements_list_matrix, get.results.table, results_coefficients, cent.tend = cent.tend, quantiles = quantiles, comparisons = comparisons, match_call = match_call, is.distribution = is.distribution)
#         names(table_matrix) <- elements_list_matrix

#         ## Check if there are any other results to output
#         if(length(results) > 1) {
#             coefficients_matrix <- which(unlist(lapply(models_results[[1]][[1]], class)) == "matrix")
#             other_results <- results[-which(results == "coefficients")]
#             ## Extracting the other elements
#             results_list <- lapply(models_results, lapply, `[[`, -coefficients_matrix)
#             ## Rename and relist the elements
#             elements_names <- names(models_results[[1]][[1]][-coefficients_matrix])
#             results_list <- lapply(results_list, lapply, relist.names, elements_names)

#             ## Creating the tables for each element in the list
#             elements_list_list <- as.list(names(results_list[[1]][[1]]))
#             table_list <- lapply(elements_list_list, get.results.table, results_list, cent.tend = cent.tend, quantiles = quantiles, comparisons = comparisons, match_call = match_call, is.distribution = is.distribution)
#             names(table_list) <- elements_list_list

#             table_matrix <- append(table_matrix, table_list)
#         }

#         ## Apply p-adjust (if necessary)
#         if(!is.null(data$correction)) {
#             table_matrix$`Pr(>|t|)` <- apply(table_matrix$`Pr(>|t|)`, 2, p.adjust, method = data$correction)
#         }

#     } else {
#         ## Creating the table for the first model
#         table_matrix <- models_results[[1]][[1]][[1]]
#         ## Creating the table for the other models
#         table_tmp <- matrix(unlist(lapply(models_results[-1], lapply, `[[`, "coefficients"), use.names = FALSE), ncol = ncol(table_matrix), byrow = TRUE)
#         ## Combining the tables
#         table_matrix <- rbind(table_matrix, table_tmp)
#         ## Removing the intercept
#         table_matrix <- table_matrix[-1,]
#         ## Adding the rownames
#         rownames(table_matrix) <- comparisons
#         ## Check if there are any other results to output
#         if(length(results) > 1) {
#             coefficients_matrix <- which(unlist(lapply(models_results[[1]][[1]], class)) == "matrix")
#             ## Extracting the other elements
#             results_list <- as.matrix(unlist(lapply(models_results, lapply, `[[`, -coefficients_matrix)))
#             ## Adding the elements names
#             colnames(results_list) <- results[-which(results == "coefficients")]
#             ## Combing it to the table
#             table_matrix <- cbind(table_matrix, results_list)
#         }

#         ## Apply p-adjust (if necessary)
#         if(!is.null(data$correction)) {
#             table_matrix[,which(colnames(table_matrix) == "Pr(>|t|)")] <- p.adjust(table_matrix[,which(colnames(table_matrix) == "Pr(>|t|)")], method = data$correction)
#         }
#     }

#     ## Getting the initial intercepts
#     if(is.distribution == TRUE) {
#         ## Get the first intercept
#         initial_intercept <- lapply(lapply(models_results, lapply, `[[`, 1), lapply, matrix.to.list, no.intercept = FALSE)[1]
#         ## Creating the tables for each element in the matrices
#         elements_list_matrix <- as.list(names(initial_intercept[[1]][[1]]))
#         ## Remove the NAs (i.e. slopes)
#         elements_list_matrix <- elements_list_matrix[-which(is.na(elements_list_matrix))]
#         intercept_list <- lapply(elements_list_matrix, get.results.table, initial_intercept, cent.tend = cent.tend, quantiles = quantiles, comparisons = comparisons[1], match_call = match_call, is.distribution = is.distribution)
#         ## Convert list into a matrix
#         initial_intercept <- matrix(unlist(intercept_list), ncol = ncol(intercept_list[[1]]), nrow = length(elements_list_matrix), byrow = TRUE,
#             dimnames = list(c(elements_list_matrix), c(colnames(intercept_list[[1]]))))

#         ## Add the predicted
#         predicted_intercept <- get.results.table(NULL, intercepts_results[-1], cent.tend = cent.tend, quantiles = quantiles, comparisons = comparisons[-1], match_call = match_call, is.distribution = is.distribution)
#         intercept_matrix <- list("Initial" = initial_intercept, "Predicted" = predicted_intercept)
#     } else {
#         ## Get the first intercept
#         initial_intercept <- models_results[[1]][[1]][[1]]
#         ## Add the other intercepts (estimated)
#         intercept_matrix <- matrix(NA, nrow = (length(intercepts_results)-1), ncol = ncol(initial_intercept))
#         intercept_matrix[,1] <- unlist(intercepts_results[-1])
#         ## Bind the two tables
#         intercept_matrix <- rbind(initial_intercept, intercept_matrix)
#         ## Remove the slope
#         intercept_matrix <- intercept_matrix[-2,]
#         ## Add rownmaes
#         rownames(intercept_matrix) <- comparisons
#     }

#     ## Combining the tables
#     results_out <- list("Slopes" = table_matrix, "Intercepts" = intercept_matrix)

#     return(results_out)
# }