#FUNCTIONS FOR SORT.DISPRITY

#Remove nulls from a list
clean.list <- function(list) {
    nulls <- unlist(lapply(list, is.null))
    return(list[!nulls])
}

#Recursive sorting
recursive.sort <- function(data, sort) {
    return(data[sort])
}