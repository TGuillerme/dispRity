#' @title Covar projection analyses wrapper
#'
#' @description Wrapper function for a covar projection analyses on dispRity objects
#'
#' @param data a \code{dispRity} object containing a \code{$covar} component(e.g. from \code{\link{MCMCglmm.subsets}})
#' @param type either \code{"groups"} for the projections between groups or \code{"elements"} for the projections of elements onto groups.
#' @param base optional, a specific group to project the elements or the groups onto. If left empty, the groups are projected onto each other in a pairwise manner and the elements are projected onto their respective groups.
#' @param sample optional, one or more specific posterior sample IDs (is ignored if n is used) or a function to summarise all axes.
#' @param n optional, a random number of covariance matrices to sample (if left empty, all are used).
#' @param major.axis which major axis to use (default is \code{1}; see \code{\link{axis.covar}} for more details).
#' @param level the confidence interval to estimate the major axis (default is \code{0.95}; see \code{\link{axis.covar}} for more details)).
#' @param output which values to output from the projection. By default, the three values \code{c("position", "distance", "degree")} are used to respectively output the projection, rejection and angle values (see \code{\link{projections}} for more details).
#' @param verbose logical, whether to be verbose (\code{TRUE}) or not (\code{FALSE}, default).
#' 
#' @details
#' Effectively, the wrapper runs either of the following function (simplified here):
#' \itemize{
#'      \item if \code{type = "groups"}: \code{dispRity(data, metric = as.covar(projections.between), between.groups = TRUE, )} for the projections group in \code{data} onto each other. 
#'      \item if \code{type = "elements"}: \code{dispRity(data, metric = as.covar(projections), ...)} for the projections of each element in \code{data} onto their main axis.
#' }
#' 
#' If \code{base} is specified:
#' \itemize{
#'      \item \code{type = "groups"} will run pairs elements each subset and \code{base} (instead of the full pairwise analyses).
#'      \item \code{type = "elements"} will run the projection of each subset onto the major axis from \code{base} rather than its own.
#' }
#' 
#' @returns
#' A \code{list} of \code{dispRity} objects corresponding to each projection value from \code{output}.
#' To visualise/summarise each element you can either select them by name (e.g. \code{output$position}) or by ID (e.g. \code{output[[1]]}).
#' 
#' @examples
#' 
#' ## TODO:
#' example
#' 1 = 2
#' 
#' 
#' 
#' 
#'
#' @seealso \code{\link{projections}} \code{\link{projections.elements}} \code{\link{axis.covar}} \code{\link{dispRity}} \code{\link{MCMCglmm.subsets}}
#' 
#' @author Thomas Guillerme
#' @export

dispRity.covar.projections <- function(data, type, base, sample, n, major.axis = 1, level = 0.95, measure = c("position", "distance", "degree"), verbose = FALSE) {

    ## Check class data (dispRity)
    check.class(data, "dispRity")
    if(is.null(data$covar)) {
        stop.call(match_call$data, " must contain a $covar component.\nSee MCMCglmm.subsets.")
    }
 
    ## Check method type (c("elements", "groups"))
    check.method(type, c("elements", "groups"))

    ## base (optional)
    if(!missing(base)) {
        check.subsets(base, data)
    }

    ## Check method measure = c("position", "distance", "degree")
    check.method(measure, c("position", "distange", "degree"))





    return("prototype")









    # ## Check for sample/n
    # if(missing(sample)) {
    #     if(missing(n)) {
    #         ## Get a random number of covar samples (TODO: GET THIS FROM THE CALL)
    #         sample <- 1:length(data$covar[[1]])
    #     } else {
    #         ## Get all the covar samples (TODO: GET THIS FROM THE CALL)
    #         sample <- sample.int(n = length(data$MCMCglmm$covars[[1]]), size = n)
    #     }
    # }

    # ## 1 - get major axis
    # if(verbose) message("calculating the major axis:...", appendLF = FALSE)
    # major_axes <- axis.covar(data, sample = sample, axis = major.axis, level = level)
    # # major_axes <- axis.covar(data)
    # if(verbose) message("Done.")

    # ## 2 - get the data
    # if(!missing(n) || !missing(sample)) {
    #     data$MCMCglmm$covars <- get.covar(data, sample = sample)
    # }

    # ## A - Type elements:
    # if(type == "elements") {
    #     wrap.dispRity.elements <- function(measure, data, list_of_pairs, verbose) {

    #         if(verbose) {
    #             message("PLACEHOLDER:...")

    #             results <- dispRity.covar(data, metric = projections.covar, elements.groups = list_of_pairs, measure = measure)$disparity

    #             message("Done.")
    #             return(results)
    #         } 

    #         ## Use normal dispRity here
    #         # dispRity.covar(data, metric = projections.covar, elements.groups = list_of_pairs, measure = measure, dimensions = data$call$dimensions, point1 = axes[1, ], point2 = axes[2, ])$disparity
    #         return(dispRity.covar(data, metric = projections.covar, elements.groups = list_of_pairs, measure = measure)$disparity)
    #     }

    #     ## 2 - get the groups
    #     if(missing(base)) {
    #         list_of_pairs <- unlist(apply(combn(1:n.subsets(data), 2), 2, list), recursive = FALSE)
    #     } else {
    #         base_id <- which(names(size.subsets(data)) == base)
    #         list_of_pairs <- lapply(as.list((1:n.subsets(data))[-base_id]), function(x,y) c(x, y), y = base_id)
    #     }

    #     ## Get all results
    #     results <- lapply(as.list(measure), wrap.dispRity.elements, data, list_of_pairs, verbose)

    #     ## Remove the elements part
    #     results <- lapply(results, lapply, function(x) {x$elements <- NULL; return(matrix(unlist(x), nrow = 1))})
    #     names(results) <- measure
    # }

    # ## B - Type groups
    # if(type == "groups") {        
    #     ## Wrapper for groups function
    #     wrap.dispRity.groups <- function(data, axes, measure, verbose) {

    #         if(verbose) message(".", appendLF = FALSE)

    #         ## Generating the function for fast disparity
    #         make.fun <- function(measure, axes) {
    #             return(function(matrix) projections.fast(matrix, point1 = axes[1,], point2 = axes[2,], measure = measure))
    #         }
    #         lapply.axes <- function(axis, measure, group, space) {
    #             metric <- make.fun(measure, axis)
    #             return(dispRity.fast(group, space, metric))
    #         }
    #         clean.results <- function(results, measure) {
    #             output <- lapply(1:length(results[[1]]), function(out) do.call(cbind, lapply(results, `[[`, out)))
    #             names(output) <- measure
    #             return(output)
    #         }

    #         ## Get the groups
    #         groups <- lapply(data$subsets, function(subset, data) return(1:nrow(data$matrix[[1]]) %in% subset$element), data = data)
    #         space <- data$matrix[[1]][, data$call$dimensions]

    #         ## Run the fast function
    #         results <- lapply(axes, lapply.axes, measure, groups[[1]], space)
    #         results_matrices <- clean.results(results, measure)
    #         return(results_matrices)
    #     }

    #     if(verbose) message("calculating projections:", appendLF = FALSE)

    #     ## Select the group and axes IDs
    #     if(missing(base)) {
    #         group_id <- axes_id <- 1:n.subsets(data)
    #     } else {
    #         ## Select the group IDs
    #         if(!incl.base) {
    #             group_id <- which(names(size.subsets(data)) != base)
    #         } else {
    #             group_id <- 1:n.subsets(data)
    #         }
    #         ## Select the axes IDs
    #         axes_id  <- which(names(size.subsets(data)) == base)
    #         axes_id  <- rep(axes_id, length(group_id))
    #     }

    #     ## Select the groups and axes
    #     grouped_data <- sapply(group_id, function(x, data) get.subsets(data, x), data = data, simplify = FALSE)
    #     grouped_axes <- sapply(axes_id, function(x, major_axes) major_axes[[x]], major_axes = major_axes, simplify = FALSE)

    #     ## Running the disparity calculations
    #     results <- mapply(wrap.dispRity.groups, grouped_data, grouped_axes, MoreArgs = list(measure = measure, verbose = verbose), SIMPLIFY = FALSE)

    #     if(verbose) message("Done.", appendLF = TRUE)

    #     ## Standardise the results (list of measure containing list of groups)
    #     reorder.results <- function(one_measure, results) {return(lapply(results, function(x, one_measure) x[[one_measure]], one_measure = one_measure))}
    #     results <- lapply(as.list(1:length(measure)), reorder.results, results = results)
    #     ## Name the groups
    #     names(results) <- measure
    #     results <- lapply(results, function(x, names) {names(x) <- names; return(x)}, names = names(size.subsets(data))[group_id])
    # }

    # if(dispRity.out) {
    #     ## Select the data subset for correct disparity display
    #     if(type == "elements") {
    #         sub_data <- get.subsets(data, subsets = unique(unlist(list_of_pairs)))
    #     } else {
    #         sub_data <- get.subsets(data, subsets = group_id)
    #     }
    #     ## Make into a dispRity object
    #     output <- list()
    #     for(one_measure in 1:length(measure)) {
    #         output[[one_measure]] <- dispRitize(results[[one_measure]], sub_data,
    #                                            name = measure[[one_measure]],
    #                                            fun = ifelse(type == "elements", projections.covar, dispRity::projections),
    #                                            type = type)
    #     }
    #     names(output) <- measure
    #     return(output)
    # } else {
    #     ## Raw results
    #     return(results)
    # }
}