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
#' @seealso \code{\link{projections}} \code{\link{projections.between}} \code{\link{axis.covar}} \code{\link{dispRity}} \code{\link{MCMCglmm.subsets}}
#' 
#' @author Thomas Guillerme
#' @export

dispRity.covar.projections <- function(data, type, base, sample, n, major.axis = 1, level = 0.95, output = c("position", "distance", "degree"), verbose = FALSE) {

    ## Check class data (dispRity)
    check.class(data, "dispRity")
    if(is.null(data$covar)) {
        stop.call(match_call$data, " must contain a $covar component.\nSee MCMCglmm.subsets.")
    }
 
    ## Check method type (c("elements", "groups"))
    check.method(type, c("elements", "groups"), msg = "type must be")

    ## base (optional)
    if(!missing(base)) {
        check.subsets(base, data)
    }

    ## output
    check.method(output, c("position", "distance", "degree"), "output must be")

    ## Check for sample/n
    if(missing(sample)) {
        if(missing(n)) {
            ## Get a random number of covar samples
            sample <- seq_len(length(data$covar[[1]]))
        } else {
            ## Get all the covar samples
            sample <- sample.int(n = length(data$covar[[1]]), size = n)
        }
    }
    ## Resampling the data
    data_sub <- data
    data_sub$covar <- get.covar(data, sample = sample)
    data_sub$call$bootstrap[[1]] <- length(sample)

    ## Calculating the major axes
    # if(verbose) message("Calculating the major axis:...", appendLF = FALSE)
    # major_axes <- axis.covar(data_sub, axis = major.axis, level = level)
    # if(verbose) message("Done.")


    ## Different analyses:
    if(type == "groups") {
        ## Get the list of pairs
        if(missing(base)) {
            ## Get the pairwise list
            list_of_pairs <- unlist(apply(combn(1:length(data$subsets), 2), 2, list), recursive = FALSE)
        } else {
            ## Get the list paired with the id
            base_id <- which(names(data$subsets) == base)
            list_of_pairs <- sapply((1:n.subsets(data))[-base_id], function(x, base_id) c(x, base_id), base_id = base_id, simplify = FALSE)
        }

        ## Get the subset names
        subset_names <- names(data$subsets)
        if(length(to_correct <- grep(":", subset_names)) > 0) {
            warning(paste0("The subset name", ifelse(length(to_correct) > 1, "s", ""), ": ", paste(subset_names[to_correct], collapse = ", "), ifelse(length(to_correct) > 1, " were ", " was "), "changed to ", paste(gsub(":", ";", subset_names)[to_correct], collapse = ", "), ". The \":\" character is reserved for between groups comparisons."))
            subset_names <- paste(gsub(":", ";", subset_names))
        }
        names(list_of_pairs) <- unlist(lapply(list_of_pairs, function(pair, names) paste0(names[pair], collapse = ":"), names = subset_names))

        ## Make the projection.between.fast function
        projections.between.fast <- projections.between
        body(projections.between.fast)[[length(body(projections.between))]][[2]][[2]][[1]] <- substitute(projections.fast)
        body(projections.between.fast)[[length(body(projections.between))]][[2]][[3]] <- NULL

        ## Update the verboseness
        decompose.VCV.internal <- decompose.VCV
        if(verbose) {
            body(decompose.VCV.internal)[[2]] <- substitute(message(".", appendLF = FALSE))
        }

        ## Use decompose.VCV directly
        if(verbose) message("Calculating projections:", appendLF = FALSE)
        disparity_tmp <- lapply(list_of_pairs, decompose.VCV.internal, fun = as.covar(projections.between.fast), data = data_sub, use_array = FALSE, use_tree = FALSE, measure = output)
        # if(length(output) == 1) {
        #     disparity_tmp <- lapply(disparity_tmp, function(X) X[1,, drop = FALSE])
        # }
        disparity_tmp <- lapply(disparity_tmp, function(X) apply(X, c(1,2), function(x) return(unlist(x)[2])))
        if(verbose) message("Done.")
        ## Get the call updated
        update_call <- list(metrics = list())
        get.call <- function(metric) return(match.call()$metric)
        update_call$metrics$name <- get.call(as.covar(projections.between))
        update_call$metrics$fun <- list(as.covar(projections.between))
        update_call$metrics$between.groups <- TRUE
    }

    ## B - Type groups
    # if(type == "elements") {
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

    ## DispRitize the results
    full_out <- list()
    for(i in 1:length(output)) {
        full_out[[i]] <- data
        full_out[[i]]$disparity <- lapply(disparity_tmp, function(X, i)return(X[i, , drop = FALSE]), i = i)
        full_out[[i]]$call$disparity <- update_call
    }
    names(full_out) <- names(output)
    return(full_out)
}