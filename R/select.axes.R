#' @title Selects ordination axes
#'
#' @description Selects the axes required to explain a cumulative threshold amount of variance in an ordination (e.g. > 95\%).
#'
#' @param data The trait space to analyse. This can be either a \code{"matrix"}, \code{"prcomp"}, \code{"princomp"} or a \code{"dispRity"} object.
#' @param group Optional, either a \code{list} of row numbers or names to be used as different groups or a \code{data.frame} with the same \eqn{k} elements as in \code{data} as rownames. If \code{data} is a \code{"dispRity"} object that already contains groups, the \code{group} argument is recycled.
#' @param threshold The arbitrary threshold amount of variance (by default this is \code{0.95}).
#' @param inc.threshold Logical, whether to output the axes that contain the threshold value (\code{TRUE}; default) or not (\code{FALSE}). See details.
#' 
#' , i.e. the axes necessary to include at least the threshold value
#' 
#' @details
#' If \code{inc.threshold = TRUE}, the returned axes are the ones that contains at least the threshold value (e.g. if the threshold is \code{0.95}, all the returned axes contain at least \code{0.95} of the variance, potentially more). If \code{inc.threshold = FALSE}, the returned axes are the ones before reaching this threshold (e.g. the cumulative variance returned is strictly less or equal to \code{0.95}).
#' 
#' @return
#' A \code{"dispRity"}, \code{"axes"} object that can be printed, summarised and plot through generic \code{print}, \code{summary} and \code{plot} functions.
#' The object is a list containing:
#' \itemize{
#'      \item \code{$dimensions}: the maximum number of dimensions selected across all groups;
#'      \item \code{$dim.list}: the selected dimensions per group;
#'      \item \code{$var}: the variance per axes per group;
#'      \item \code{$scaled.var}: the variance scaled variance per axes per group;
#'      \item \code{$cumsum.var}: the cumulative scaled variance per axes per group;
#'      \item \code{$call}: a list containing the \code{$threshold} value and the \code{$inc.threshold} option used.
#' }
#' 
#' @examples
#' ## Ordinating the USArrests dataset
#' ordination <- princomp(USArrests, cor = TRUE)
#' ## Which dimensions to select?
#' (selected <- select.axes(ordination))
#' ## The selected dimensions
#' selected$dimensions
#' ## Visualising the results
#' plot(selected)
#'
#' ## Same but by grouping the data into three groups
#' states_groups <- list("Group1" = c("Mississippi","North Carolina",
#'                                    "South Carolina", "Georgia", "Alabama",
#'                                    "Alaska", "Tennessee", "Louisiana"),
#'                       "Group2" = c("Florida", "New Mexico", "Michigan",
#'                                    "Indiana", "Virginia", "Wyoming", "Montana",
#'                                    "Maine", "Idaho", "New Hampshire", "Iowa"),
#'                       "Group3" = c("Rhode Island", "New Jersey", "Hawaii",
#'                                    "Massachusetts"))
#' (selected <- select.axes(ordination, group = states_groups))
#' ## Note that the required number of axes is now 4 (instead of 3)
#' plot(selected)
#' 
#' ## Loading some example dispRity data
#' data(demo_data)
#' ## How many axes are required to explain 99% of the variance
#' ## for each group in the Healy et al 2019 data?
#' (how_many <- select.axes(demo_data$healy, threshold = 0.99))
#' summary(how_many)
#' plot(how_many)
#' 
#'
#' @seealso \code{\link{custom.subsets}}
#' 
#' @author Thomas Guillerme
#' @export

# data(demo_data)
# data <- demo_data$healy$matrix[[1]]
# group <- unlist(demo_data$wright$subsets, recursive = FALSE)
# group <- lapply(group, c)


select.axes <- function(data, group, threshold = 0.95, inc.threshold = TRUE) {

    match_call <- match.call()

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
    base_group <- 1:nrow(trait_space[[1]])
    
    ## Set up the groups
    if(missing(group)) {
        if(data_class == "dispRity") {
            ## Extracting the groups from the dispRity object
            group <- lapply(data$subsets, function(x) return(c(x$elements)))
        } else {
            group <- list(whole_space = base_group)
        }
    }
        
    ## Sanitize the group variable
    group_class <- check.class(group, c("matrix", "data.frame", "list", "phylo"))
    if(group_class == "phylo") {
        ## Saving the tree for export
        tree <- group
    }
    ## Set the group.list
    group_list <- set.group.list(group, trait_space, group_class)

    ## Replace nulls by NAs in groups
    group_list <- lapply(group_list, function(x) if(is.null(x)){return(NA)}else{return(x)})

    ## Find empty groups
    if(any(empty_groups <- is.na(group_list))) {
        warning(paste0("The following subset", ifelse(sum(empty_groups) > 1, "s are ", " is "), "empty: ", paste0(names(which(empty_groups)), collapse = ", "), "."))
    }

    ## Check the group list
    group_list <- check.group.list(group_list, trait_space, group_class, match_call)

    ## Add the base group (if not present)
    if(!any(unlist(lapply(group_list, length)) == length(base_group))) {
        group_list$whole_space <- base_group    
    }
    
    ## Get the variance per group
    group_var <- lapply(group_list, function(row,data) apply(data[[1]][row,, drop = FALSE], 2, var), data = trait_space)

    ## Get the scaled variance per group
    group_sumvar <- lapply(group_var, function(x) x/sum(x))

    ## Get the cumulative scaled variance per group
    group_cumsumvar <- lapply(group_sumvar, function(x) cumsum(x))

    ## Get the axis thresholds
    if(!inc.threshold) {
        group_axes <- lapply(group_cumsumvar, function(x, threshold) unname(which(x <= threshold)), threshold)
    } else {
        group_axes <- lapply(group_cumsumvar, function(x, threshold) unname(c(1, which(x <= threshold) + 1)), threshold)
    }

    ## Dimensions lengths per groups
    dim_lengths <- unlist(lapply(group_axes, length))

    output <- list(
        dimensions = 1:dim_lengths[which(dim_lengths == max(dim_lengths))[1]],
        dim.list   = group_axes,
        var        = group_var,
        scaled.var = group_sumvar,
        cumsum.var = group_cumsumvar,
        call       = list(threshold = threshold, inc.threshold = inc.threshold, colnames = colnames(trait_space[[1]])))

    class(output) <- c("dispRity", "axes")

    return(output)
}