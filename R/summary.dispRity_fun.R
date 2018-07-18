## Converts one or more CI into quantile probabilities
CI.converter <- function(CI) {
    sort(c(50-CI/2, 50+CI/2)/100)
}

# Wrapper function for summarising a single rarefaction
get.summary <- function(disparity_subsets_rare, cent.tend, quantiles) {

    output <- list()

    ## Summarising NA
    if(is.na(disparity_subsets_rare[[1]])) {
        if(!missing(cent.tend)) {
            output$cent.tend <- NA
        }
        if(!missing(quantiles)) {
            output$quantiles <- rep(NA, length(quantiles) * 2)
        }
        return(output)
    }

    ## Summarising normal data
    if(!missing(cent.tend)) {
        output$cent_tend <- cent.tend(as.vector(disparity_subsets_rare))
    }
    if(!missing(quantiles)) {
        output$quantiles <- quantile(as.vector(disparity_subsets_rare), probs = CI.converter(quantiles))
    }
    return(output)
}

## lapply wrapper function for summarising a single subset
lapply.summary <- function(disparity_subsets, cent.tend, quantiles) {
    return(lapply(disparity_subsets[-1], get.summary, cent.tend, quantiles))
}

## lapply wrapper for getting elements
lapply.get.elements <- function(subsets, bootstrapped = TRUE) {
    if(bootstrapped){
        return(unlist(lapply(subsets[-1], nrow)))
    } else {
        return(unlist(lapply(subsets, nrow)))
    }
}

## lapply wrapper for getting the disparity observed values
lapply.observed <- function(disparity) {
    output <- ifelse(any(is.na(disparity$elements)), NA, c(disparity$elements))
    return(output)
}

## mapply wrapper for getting the disparity observed values
mapply.observed <- function(disparity, elements) {
    return(c(disparity, rep(NA, (length(elements)-1))))
}

## Get digits for table (shifts the decimal point to contain a maximum of four characters)
get.digit <- function(column) {
    if(max(nchar(round(column)), na.rm = TRUE) <= 4) {
        return(4 - max(nchar(round(column)), na.rm = TRUE))
    } else {
        return(0)
    }
}


round.column <- function(column, digits) {
    ## Get the digits value
    digits <- ifelse(digits != "default", digits, get.digit(as.numeric(column)))
    ## Round the table
    if(class(column) != "group") {
        return(round(as.numeric(column), digits = digits))
    } else {
        return(round(as.numeric(as.character(column)), digits = digits))
    }
}


## Function for digits the results
digits.fun <- function(results_table, digits, model.test = FALSE) {

    ## model.test
    start_column <- ifelse(model.test, 1, 3)

    ## Apply the digits
    rounded_table <- as.matrix(results_table[,c(start_column:ncol(results_table))])
    rounded_table <- apply(rounded_table, 2, round.column, digits)
    results_table[,c(start_column:ncol(results_table))] <- rounded_table

    return(results_table)
}


## match parameters lapply (for model.test summaries)
match.parameters <- function(one_param, full_param) {
    ## Which parameters are present:
    available_param <- full_param %in% names(one_param)
    if(any(!available_param)) {
        ## Making the missing parameters NAs
        output_param <- rep(NA, length(full_param))
        names(output_param) <- full_param
        output_param[available_param] <- one_param[order(match(names(one_param), full_param))]
        return(output_param)
    } else {
        return(one_param)
    }
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

# summary.seq.test <- function(data, quantiles, cent.tend, recall, digits, results, match_call) {
    
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