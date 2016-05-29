#transforming test into a lapply from a given list of comp for multiple distributions
test.list.lapply.distributions <- function(list_of_comp, data, test, ...) {
    
    test.mapply <- function(pair_comparison, data, test, ...) {
        return(mapply(test, data[[pair_comparison[[1]]]], data[[pair_comparison[[2]]]], MoreArgs = ..., SIMPLIFY = FALSE))
    }

    output <- lapply(list_of_comp, test.mapply, data, test, ...)
    return(output)
}

#Creating a list of sequences
set.sequence <- function(length) {
    #Sequence of only 2
    if(length==2) {
        output <- matrix(data=c(1,2), nrow=2, byrow=TRUE)
    } else {
    #sequence of more
        output <- matrix(data=c(1:(length-1), 2:length), nrow=2, byrow=TRUE)
    }
    return(output)
}

#convert a list from character to numeric
convert.to.numeric <- function(list, object) {
    return(lapply(list, match, names(object)))
}

#convert a list from numeric to character
convert.to.character <- function(list, object) {
    #Getting the names (character)
    names.fun <- function(list, object) {
        return(names(object[c(list)]))
    }
    #Applying to the list
    return(lapply(list, names.fun, object))
}


#Convert a list into a table (for aov)
list.to.table <- function(extracted_data, style = "factor") {
    #function for repeating the extracted_data names
    rep.names <- function(name, series) {
        return(rep(name, series))
    }

    #Get the list of names
    names_list <- as.list(names(extracted_data))
    #If no names, just get list numbers
    if(length(names_list) == 0) {
        names_list <- as.list(seq(from = 1, to = length(extracted_data)))
    }
    series_length <- unlist(lapply(extracted_data, length), recursive = FALSE)

    #Create the data.frame
    output <- data.frame("data" = unlist(extracted_data), row.names = NULL, "series" = unlist(mapply(rep.names, names_list, series_length, SIMPLIFY = FALSE)))

    #Transform factors to numeric
    if(style == "binomial") {
        output$factor <- as.numeric(output$factor)-1
    }

    return(output)
}

htest.to.vector <- function(htest, print) {
    #print is a vector of htest elements to print
    #lapply fun
    get.element <- function(print, htest) {
        return(htest[grep(print, names(htest))][[1]])
    }
    return(unlist(lapply(print, get.element, htest)))
}

#Set the list of comparisons
set.comparisons.list <- function(comp, extracted_data, comparisons) {
    options(warn = -1)
    if(comp == "custom") {
        #get the lit of series to compare
        comp_series <- comparisons
    }

    if(comp == "pairwise") {
        #Get the pairs of series
        comp_series <- combn(1:length(extracted_data), 2)
        #convert pair series table in a list of pairs
        comp_series <- unlist(apply(comp_series, 2, list), recursive = FALSE)
    }

    if(comp == "sequential") {
        #Set the list of sequences
        comp_series <- set.sequence(length(extracted_data))
        #convert seq series in a list of sequences
        comp_series <- unlist(apply(comp_series, 2, list), recursive = FALSE)
    }

    if(comp == "referential") {
        #Set the list of comparisons as a matrix
        matrix_data <- c(rep(1, length(extracted_data) - 1), seq(from = 2, to = length(extracted_data)))
        comp_series <- matrix(matrix_data, ncol = (length(extracted_data) - 1), byrow = TRUE)
        #convert pair series table in a list of pairs
        comp_series <- unlist(apply(comp_series, 2, list), recursive = FALSE)
    }
    options(warn = 0)

    return(comp_series)
}

#Save the comparisons list
save.comparison.list <- function(comp_series, extracted_data) {
    #Saving the list of comparisons
    comparisons_list <- convert.to.character(comp_series, extracted_data)
    comparisons_list <- unlist(lapply(comparisons_list, paste, collapse = " - "))
    return(comparisons_list)
}

#Function for lapplying aov type functions
lapply.aov.type <- function(data, test, ...) {
    return(test(data ~ series, data = data, ...))
}


#Calculate the central tendency and the quantiles from a table of results
get.quantiles.from.table <- function(table, con.cen.tend, conc.quantiles, ...) {
    return(t(rbind(apply(table, 1, con.cen.tend, ...), apply(table, 1, quantile, probs = conc.quantiles, ...))))
}

#Returning a table of numeric values
output.numeric.results <- function(details_out, name, comparisons_list, conc.quantiles, con.cen.tend) {
    #Transforming list to table
    table_temp <- do.call(rbind.data.frame, details_out)

    #Calculate the quantiles and the central tendency
    if(!missing(conc.quantiles) && !missing(con.cen.tend)) {
        table_out <- get.quantiles.from.table(table_temp, con.cen.tend, conc.quantiles)
    } else {
        table_out <- table_temp
    }

    #Getting col names
    colnames(table_out)[1] <- name
    #Getting row names (the comparisons)
    row.names(table_out) <- comparisons_list

    return(table_out)
}

#Returning a table for htests
output.htest.results <- function(details_out, comparisons_list, conc.quantiles, con.cen.tend) {
    #Getting the test elements
    test_elements <- unique(unlist(lapply(details_out, lapply, names)))
    #Selecting the numeric (or) integer elements only
    test_elements <- test_elements[grep("numeric|integer", unlist(lapply(as.list(details_out[[1]][[1]]), class)))]
    #Remove null.value and the estimates
    remove <- match(c("null.value", "conf.int", "estimate"), test_elements)
    if(any(is.na(remove))) {
    remove <- remove[-which(is.na(remove))]
    }
    if(length(remove) > 0) {
        test_elements <- test_elements[-remove]
    }

    #Lapply function for getting the test elements
    lapply.output.test.elements <- function(test_element, details_out, comparisons_list, conc.quantiles, con.cen.tend) {
        if(!missing(conc.quantiles) && !missing(con.cen.tend)) {
            return(output.numeric.results(lapply(lapply(details_out, lapply, htest.to.vector, print = test_element), unlist), test_element, comparisons_list, conc.quantiles, con.cen.tend))
        } else {
            return(output.numeric.results(lapply(lapply(details_out, lapply, htest.to.vector, print = test_element), unlist), test_element, comparisons_list))
        }
    }
    
    #Get the results
    if(!missing(conc.quantiles) && !missing(con.cen.tend)) {
        table_out <- lapply(as.list(test_elements), lapply.output.test.elements, details_out, comparisons_list, conc.quantiles, con.cen.tend)
    } else {
        table_out <- lapply(as.list(test_elements), lapply.output.test.elements, details_out, comparisons_list)
    }

    return(table_out)
}

#Handling output for aov multiple tests
output.aov.results <- function(details_out, conc.quantiles, con.cen.tend) {
    #Getting the summaries
    summaries <- lapply(details_out, summary)
    
    #Transforming the list 
    list_of_results <- list()
    for(element in 1:length(summaries[[1]][[1]])) {
        list_of_results[[element]] <- matrix(unlist(lapply(lapply(summaries, `[[`, 1), `[[`, element)), nrow = length(summaries[[1]][[1]][[element]]),
            dimnames = list(c("series", "Residuals")))
    }

    #Get the quantiles
    list_of_results <- lapply(list_of_results, get.quantiles.from.table, con.cen.tend, conc.quantiles, na.rm = TRUE)

    #Name the elements
    for(element in 1:length(summaries[[1]][[1]])) {
        colnames(list_of_results[[element]])[[1]] <- names(summaries[[1]][[1]])[[element]]
    }    

    return(list_of_results)
}