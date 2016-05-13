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

#Gets results table from results elements
get.results.table <- function(result_element, cent.tend, quantiles, comparisons, match_call) {
    #Central tendency
    results_table <- matrix(data = lapply(result_element, cent.tend), ncol = 1, dimnames = list(comparisons))
    #colnames(results_table) <- paste(match_call$cent.tend)
    colnames(results_table) <- "mean" ; warning("DEBUG")

    #Quantiles
    results_quantiles <- lapply(result_element, quantile, probs = CI.converter(quantiles))

    #Create table
    quantiles_res <- unlist(lapply(result_element, quantile, probs = CI.converter(quantiles)))
    results_table <- cbind(results_table, matrix(data = quantiles_res, nrow = length(comparisons), dimnames = list(c(), c(unique(names(quantiles_res))))))

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

    #SAVING THE RESULTS
    models_results <- lapply(data$models, lapply, save.results.seq.test, results)
    intercepts_results <- lapply(data$intercepts, unlist)
    comparisons <- unique(unlist(names(data$models)))

    #Getting the slopes

    #Needs to transform the models results into a list of elements as follows:
    #$Estimate
    #$Std. Error
    #$aic
    #$ect.
    #The for each element use:
    #results_out <- lapply(list_of_elements, get.results.table)
    #names(results_out) <- names(list_of_elements[[1]])

    # elements <- models_results[[1]]

    # slopes_lists <- lapply(models_results, `[[`, elements)





    #Calculating the central tendency
    results_cent <- unlist(lapply(BSresults_unl, cent.tend))

    #Calculate the quantiless
    results_quantiles <- lapply(BSresults_unl, quantile, probs = CI.converter(quantiles))

    #Add to the result table
    results_table <- cbind(results_table, matrix(data = unlist(results_quantiles), ncol = length(quantiles)*2, byrow = TRUE))


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

    #Saving the slopes
    Slope_results <- Matrix_template
    Slope_results[1,] <- first_model_results$coefficients[2,]
    colnames(Slope_results) <- names(first_model_results$coefficients[2,])

    #Adding the other slopes
    if(length(seq_series) > 1) {
        #Adding the slopes
        for(model in 2:length(seq_series)) {
            Slope_results[model,] <- models_results[[model]]$coefficients
        }
    }

    #correction of the slopes p-values
    if(!missing(correction)) {
        Slope_results[,which(colnames(Slope_results) == "Pr(>|t|)")] <- p.adjust(Slope_results[,which(colnames(Slope_results) == "Pr(>|t|)")], correction)
    }

    #Combining the tables
    results_out <- list("Intercept" = Intercept_results, "Slope" = Slope_results)



}
