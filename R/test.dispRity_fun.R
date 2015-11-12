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

#Convert a list into a table (for aov)
list.to.table <- function(extracted_data) {
    #function for repeating the extracted_data names
    mapply.rep.names <- function(name, series) {
        return(rep(name, length(series)))
    }

    #Get the list of names
    names_list<-as.list(names(extracted_data))

    #Create the data.frame
    output <- data.frame("data"=unlist(extracted_data), row.names=NULL, "factor"=unlist(mapply(mapply.rep.names, names_list, extracted_data, SIMPLIFY=FALSE)))
    return(output)
}
