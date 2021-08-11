#' @title MCMCglmm.subsets
#'
#' @description Creating a dispRity object from a MCMCglmm posterior output
#'
#' @param data The data used for the MCMCglmm model
#' @param posteriors A MCMCglmm object, the posteriors of the model
#' @param group Optional, which group to include from the posteriors (if left empty the residuals and random terms are used)
#' @param tree Optional, the tree(s) used in the MCMCglmm analyses
#' @param rename.groups optional, a vector of group names for renaming them.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

MCMCglmm.subsets <- function(data, posteriors, group, tree, rename.groups) {

    ## Cleaning then checking the data (i.e. only removing the values)
    data_class <- check.class(data, c("data.frame", "matrix"))
    if(data_class == "data.frame") {
        ## Remove potential non-numeric columns
        col_classes <- sapply(1:ncol(data), function(col, dat) class(data[,col]), dat = data)
        numerics <- (col_classes %in% c("numeric", "integer"))
        ## Check for non-numerics
        if(!any(numerics)) {
            stop.call(msg = "The data does not contain any column with numeric or integer values.", call = "")
        }
        ## Clean the data
        cleaned_data <- as.matrix(data[,numerics])
        ## Is there any classification column?
        classifier <- col_classes[!numerics] %in% "factor"
        if(any(classifier)) {
            recycled_group_classifier <- which(!numerics)[which(classifier)]
        }
    }
    
    # ## Checking the posteriors
    # #check.class(posteriors, "MCMCglmm")
    # posteriors
    # ## Extracting the residuals and randoms
    # terms

    # ## Getting the groups
    # ## Extracting the group from the posteriors
    # if(missing(group)) {
    #     ## Automatically detect the groups
    # }

    # ## Renaming the groups
    # rename.groups

    # ## Adding the tree


    return(NULL)
}