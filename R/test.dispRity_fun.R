#flip tests for referential lapply (x becomes fix variable and y becomes list)
flip.ref.lapply <- function(series, referential, test, ...) {
    output <- test(referential, series, ...)
    return(output)
}

#transforming test into a lapply from a given list of comp
test.list.lapply <- function(list_of_comp, data, test, ...) {
    output <- test(data[[list_of_comp[[1]]]], data[[list_of_comp[[2]]]], ...)
    return(output)
}


#transforming test into a lapply from a given list of comp for multiple distributions
test.list.lapply.distributions <- function(list_of_comp, data, test, ...) {
    
    test.mapply <- function(pair_comparison, data, test, ...) {
        return(mapply(test, data[[pair_comparison[[1]]]], data[[pair_comparison[[2]]]], MoreArgs = ..., SIMPLIFY = FALSE))
    }

    output <- lapply(list_of_comp, test.mapply, data, test, ...)
    #output <- lapply(list_of_comp, test.mapply, data, test) ; warning("DEBUG")
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
    mapply.rep.names <- function(name, series) {
        return(rep(name, length(series)))
    }

    #Get the list of names
    names_list<-as.list(names(extracted_data))

    #Create the data.frame
    output <- data.frame("data"=unlist(extracted_data), row.names=NULL, "factor"=unlist(mapply(mapply.rep.names, names_list, extracted_data, SIMPLIFY=FALSE)))

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

get.name <- function(X, htest) {
    output <- names(htest[[match(X, names(htest))]])
    if(is.null(output)) {
        return(X)
    } else {
        return(output)
    }
}


##TODO: following functions are not tested yet


#Set the list of comparisons
set.comparisons.list <- function(comp, extracted_data, comparisons) {
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

    return(comp_series)
}

#Save the comparisons list
save.comparison.list <- function(comp, comp_series, extract_data) {
    #Saving the list of comparisons
    comparisons_list <- convert.to.character(comp_series, extracted_data)
    comparisons_list <- unlist(lapply(comparisons_list, paste, collapse = " - "))
    return(comparisons_list)
}

#Returning an table of numeric values
output.numeric.results <- function(details_out, match_call, comparisons_list, conc.quantiles, con.cen.tend) {
    #Transforming list to table
    table_temp <- do.call(rbind.data.frame, details_out)

    #Calculate the quantiles and the central tendency
    if(!missing(conc.quantiles) && !missing(con.cen.tend)) {
        table_out <- t(rbind(apply(table_temp, 1, con.cen.tend), apply(table_temp, 1, quantile, probs = conc.quantiles)))
    } else {
        table_out <- table_temp
    }

    #Getting col names
    colnames(table_out)[1] <- as.expression(match_call$test)
    #Getting row names (the comparisons)
    row.names(table_out) <- comparisons_list

    return(table_out)            
}
