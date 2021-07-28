#' @title Selects ordination axes
#'
#' @description Selects the axes required to explain a cumulative threshold amount of variance in an ordination (e.g. > 95\%).
#'
#' @param data The trait space to analyse. This can be either a \code{"matrix"}, \code{"prcomp"}, \code{"princomp"} or a \code{"dispRity"} object.
#' @param group Optional, either a \code{list} of row numbers or names to be used as different groups or a \code{data.frame} with the same \eqn{k} elements as in \code{data} as rownames. If \code{data} is a \code{"dispRity"} object that already contains groups, the \code{group} argument is recycled.
#' @param threshold The arbitrary threshold amount of variance (by default this is \code{0.95}).
#' 
#' @return
#' A \code{"dispRity"}, \code{"axes"} object that can be printed, summarised and plot through generic \code{print}, \code{summary} and \code{plot} functions.
#' The object is a list containing:
#' \itemize{
#'      \item \code{$dimensions}: the selected dimensions;
#'      \item \code{$var.table}: the variance per axes table;
#'      \item \code{$cum.var.table}: the cumulative variance per axes table.
#' }
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

# data(demo_data)
# data <- demo_data$wright$matrix[[1]]
# group <- unlist(demo_data$wright$subsets, recursive = FALSE)


select.axes <- function(data, group, threshold = 0.95) {

    ## Set up the data type
    data_class <- check.class(data, c("dispRity", "matrix", "prcomp", "princomp"))
    trait_space <- switch(data_class,
                          "matrix"   = list(data),
                          "prcomp"   = list(data$x),
                          "princomp" = list(data$scores),
                          "dispRity" = data$matrix)
    ## Get the general dimensions
    n_dimensions <- ncol(trait_space[[1]])
    n_elements <- nrow(trait_space[[1]])

    ## Set up the base group (all elements)
    group_base <- list("whole_space" = matrix(1:nrow(trait_space[[1]]), ncol = 1))
    
    ## Set up the groups
    if(missing(group)) {
        if(data_class == "dispRity") {
            ## Extracting the groups from the dispRity object
            group <- lapply(data$subsets, function(x) return(x$elements))
            if(max(unlist(lapply(group, length))) != n_dimensions) {
                group <- c(group_base, group)
            }
        } else {
            ## No group (only whole space)
            group <- group_base
        }
    } else {
        ## Check if group works

    }

    # Group class can be "data.frame", "matrix", "list", "phylo"




    # Subset by data frame
    #subsets_list <- unlist(apply(group, 2, split.elements.data.frame, data[[1]]), recursive = FALSE)


    return(NULL)
}