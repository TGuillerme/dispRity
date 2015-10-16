extract.rar <- function(list, which.rare) {
    #Set up which rarefaction function
    if(is.numeric(which.rare)) {
        rare.fun<-FALSE
    } else {
        rare.fun<-TRUE
        if(which.rare == "max") which.rare.fun<-max
        if(which.rare == "min") which.rare.fun<-min
    }

    #Extracting specific a level of rarefaction
    if(rare.fun == FALSE) {
        output <- list[which.rare]
    } else {
        output <- list[which.rare.fun(1:length(list))]
    }

    #output
    return(output)
}