#Bootstrap unlisting
recursive.unlist<-function(results, is.distribution = FALSE) {
    n_series <- length(results)
    n_rare <- unlist(lapply(results, length))
    n_bootstraps <- length(results[[1]][[1]])

    series_rare_boot <- list()
    for(series in 1:n_series) {
        rare_boot <- list()
        for (rare in 1:n_rare[series]) {
            if(is.distribution == FALSE) {
                rare_boot[[rare]] <- unlist(results[[series]][[rare]])
            } else {
                rare_boot[[rare]] <- results[[series]][[rare]]
            }
        }
        series_rare_boot[[series]] <- rare_boot
    }
    return(series_rare_boot)
}


#Converts one or more CI into a quantile probabilities
CI.converter<-function(CI) {
    sort(c(50-CI/2, 50+CI/2)/100)
}

#Calculates taxonomic diversity per slice (include rarefaction)
diversity.count<-function(data) {
    if(class(data) == "matrix") {
        return(nrow(data))
    } else {
        if(class(data[[1]]) == "matrix") {
            #Get the number of rows per matrices
            output<-unlist(lapply(data, nrow))
            #Remove matrices names
            names(output) <- NULL
            return(output)
        }
        #lapply frenzy!
        return(unlist(lapply(lapply(data, lapply, lapply, nrow), lapply, unique), use.names=FALSE))
    }
}

#Get digit for table
get.digit<-function(column) {
    if(max(nchar(round(column)), na.rm = TRUE) <= 4) {
        return(4-max(nchar(round(column)), na.rm = TRUE))
    } else {
        return(0)
    }
}


#Saving results function
save.results.seq.test <- function(model, results) {
    save_out <- match(results, names(summary(model)))
    return(summary(model)[save_out])
}

#Transform a matrix (usually the coefficient results) into a list
matrix.to.list <- function(matrix, no.intercept = TRUE) {
    #Remove the intercept column (if needed)
    if(no.intercept == TRUE) {
        if(any(rownames(matrix) == "(Intercept)")) {
            matrix <- matrix[-which(rownames(matrix) == "(Intercept)"),]
        }
    }

    #Transform the matrix into a list
    output <- as.list(matrix)
    #Adding names (if necessary)
    if(is.null(names(output))) {
        names(output) <- colnames(matrix)
    }
    return(output)
}

#Relists a list (recursive) with element names
relist.names <- function(element, elements_names) {
    output <- list(element)
    names(output) = elements_names
    return(output)
}

#Gets results table from results elements
get.results.table <- function(element, results_elements, cent.tend, quantiles, comparisons, match_call, is.distribution) {
    #Get the data
    data <- lapply(lapply(results_elements, lapply, `[[`, element), unlist)
    #Central tendency
    results_table <- matrix(data = lapply(data, cent.tend), ncol = 1, dimnames = list(comparisons))
    #colnames(results_table) <- paste(match_call$cent.tend)
    colnames(results_table) <- "mean" ; warning("DEBUG")

    if(is.distribution == TRUE) {
        #Quantiles
        results_quantiles <- lapply(data, quantile, probs = CI.converter(quantiles))
        #Create table
        results_table <- cbind(results_table, matrix(data = unlist(results_quantiles), nrow = length(comparisons), byrow = TRUE, dimnames = list(c(), c(names(results_quantiles[[1]])))))
    }
    return(results_table)
}

summary.seq.test <- function(data, quantiles, cent.tend, recall, rounding, results, match_call) {
    
    #SANITIZING
    #quantiles
    check.class(quantiles, "numeric", " must be any value between 1 and 100.")
    #remove warnings
    options(warn = -1)
    if(any(quantiles) < 1) {
        stop("quantiles(s) must be any value between 1 and 100.")
    }
    if(any(quantiles) > 100) {
        stop("quantiles(s) must be any value between 1 and 100.")
    }
    options(warn = 0)

    #Check if is distribution
    is.distribution <- ifelse(length(data$models[[1]]) == 1, FALSE, TRUE)

    #Results
    check.class(results, "character") 
    #At least one must be "coefficients"
    if(is.na(match("coefficients", results))) {
        stop("At least one of the returned results must be 'coefficients'.")
    }

    #SAVING THE RESULTS
    models_results <- lapply(data$models, lapply, save.results.seq.test, results)
    intercepts_results <- lapply(data$intercepts, unlist)
    comparisons <- unique(unlist(names(data$models)))


    #Getting the slopes
    if(is.distribution == TRUE) {
        #Getting the coefficients
        #Transforming the coefficients into a list
        results_coefficients <- lapply(lapply(models_results, lapply, `[[`, any_matrix), lapply, matrix.to.list, no.intercept = TRUE)

        #Creating the tables for each element in the matrices
        elements_list_matrix <- as.list(names(results_coefficients[[1]][[1]]))
        table_matrix <- lapply(elements_list_matrix, get.results.table, results_coefficients, cent.tend = cent.tend, quantiles = quantiles, comparisons = comparisons, match_call = match_call, is.distribution = is.distribution)
        names(table_matrix) <- elements_list_matrix

        #Check if there are any other results to output
        if(length(results) > 1) {
            coefficients_matrix <- which(unlist(lapply(models_results[[1]][[1]], class)) == "matrix")
            other_results <- results[-which(results == "coefficients")]
            #Extracting the other elements
            results_list <- lapply(models_results, lapply, `[[`, -coefficients_matrix)
            #Rename and relist the elements
            elements_names <- names(models_results[[1]][[1]][-coefficients_matrix])
            results_list <- lapply(results_list, lapply, relist.names, elements_names)

            #Creating the tables for each element in the list
            elements_list_list <- as.list(names(results_list[[1]][[1]]))
            table_list <- lapply(elements_list_list, get.results.table, results_list, cent.tend = cent.tend, quantiles = quantiles, comparisons = comparisons, match_call = match_call, is.distribution = is.distribution)
            names(table_list) <- elements_list_list

            table_matrix <- append(table_matrix, table_list)
        }
    } else {
        #Creating the table for the first model
        table_matrix <- models_results[[1]][[1]][[1]]
        #Creating the table for the other models
        table_tmp <- matrix(unlist(lapply(models_results[-1], lapply, `[[`, "coefficients"), use.names = FALSE), ncol = ncol(table_matrix), byrow = TRUE)
        #Combining the tables
        table_matrix <- rbind(table_matrix, table_tmp)
        #Removing the intercept
        table_matrix <- table_matrix[-1,]
        #Adding the rownames
        rownames(table_matrix) <- comparisons
        #Check if there are any other results to output
        if(length(results) > 1) {
            coefficients_matrix <- which(unlist(lapply(models_results[[1]][[1]], class)) == "matrix")
            #Extracting the other elements
            results_list <- as.matrix(unlist(lapply(models_results, lapply, `[[`, -coefficients_matrix)))
            #Adding the elements names
            colnames(results_list) <- results[-which(results == "coefficients")]
            #Combing it to the table
            table_matrix <- cbind(table_matrix, results_list)
        }
    }

    #Getting the intercepts

   
    #Creating the saving table template
    matrix_template <- matrix(NA, nrow = length(seq_series), ncol = length(first_model_results$coefficients[1,]))
    rownames(Matrix_template) <- unlist(lapply(convert.to.character(seq_series, series), paste, collapse=" - "))

    #Saving the first intercept
    Intercept_results <- Matrix_template
    Intercept_results[1,] <- first_model_results$coefficients[1,]
    colnames(Intercept_results) <- names(first_model_results$coefficients[1,])

    #Adding the predict column
    if(length(seq_series) > 1) {
        #Empty Predict column
        Intercept_results <- cbind(rep(NA, nrow(Intercept_results)), Intercept_results)
        colnames(Intercept_results)[1] <- "Predict"
        #Added predicted intercepts (ignoring the last one that's not used)
        Intercept_results[,1] <- intercept_predict
    }
    
    #correction of the slopes p-values
    if(!missing(correction)) {
        Slope_results[,which(colnames(Slope_results) == "Pr(>|t|)")] <- p.adjust(Slope_results[,which(colnames(Slope_results) == "Pr(>|t|)")], correction)
    }

    #Combining the tables
    results_out <- list("Intercept" = Intercept_results, "Slope" = table_matrix)



}
