#' @title Covar projection analyses wrapper
#'
#' @description Wrapper function for a covar projection analyses on dispRity objects
#'
#' @param data a \code{dispRity} object containing a \code{$covar} component(e.g. from \code{\link{MCMCglmm.subsets}})
#' @param type either \code{"groups"} for the projections between groups or \code{"elements"} for the projections of elements onto groups.
#' @param base optional, a specific group to project the elements or the groups onto or a list of pairs of groups to compare (see \code{between.groups} argument in \code{\link{dispRity}}). If left empty, the groups are projected onto each other in a pairwise manner and the elements are projected onto their respective groups.
#' @param sample optional, one or more specific posterior sample IDs (is ignored if n is used) or a function to summarise all axes.
#' @param n optional, a random number of covariance matrices to sample (if left empty, all are used).
#' @param major.axis which major axis to use (default is \code{1}; see \code{\link{axis.covar}} for more details).
#' @param level the confidence interval to estimate the major axis (default is \code{0.95}; see \code{\link{axis.covar}} for more details)).
#' @param output which values to output from the projection. By default, the three values \code{c("position", "distance", "degree")} are used to respectively output the projection, rejection and angle values (see \code{\link{projections}} for more details). The argument \code{"orthogonality"} can also be added to this vector.
#' @param inc.base logical, when using \code{type = "elements"} with a supplied \code{base} argument, whether to also calculate the projections for the base group (\code{TRUE}) or not (\code{FALSE}; default).
# @param distance.method which method to use to calculate the distance (rejection). Can be either \code{"euclidean"} (default) or \code{"CI"} to change the unit vector to either the projection of the confidence interval (see details).
#' @param ... any optional arguments to pass to \code{\link{projections}} (such as \code{centre} or \code{abs}). \emph{NOTE that this function uses by default \code{centre = TRUE} and \code{abs = TRUE} which are not the defaults for \code{\link{projections}}}. 
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
# If \code{output} contains \code{"distance"}, the \code{distance.method} allows for the two following calculations of the rejections:
# \itemize{
#      \item \code{"euclidean"} (default): calculates the distance values (rejections) as true euclidean distances in the space using typically the unit vector from the space (or from the rescaled space if the optional argument (\code{...}), \code{scale = TRUE} - default - is used). With this method, a rejection (\code{"distance"}) of 1 is at the same distance from the center of the space as a projection (\code{"position"}) of 1.
#      \item \code{"CI"}: calculates the distance values (rejections) as non-euclidean distances but relative distances from the confidence interval (from the argument \code{level}). With this method, a rejection (\code{"distance"}) of 1 must be interpreted as a distance relative to the confidence interval ellipse: a value of equals that the rejection is on the confidence interval ellipse, and values above and below that value respectively mean within and without that confidence interval. With this method, a rejection of 1 is not at the same distance from the center of the space as a projection of 1 but are both on the same place relative to the confidence interval ellipse.
#}
#' 
#' @returns
#' A \code{list} of class \code{"dispRity"} and \code{"projection"} which contains \code{dispRity} objects corresponding to each projection value from \code{output}.
#' The elements of the \code{list} can be accessed and analysed individually by selecting them by name (e.g. \code{output$position}) or by ID (e.g. \code{output[[1]]}).
#' Alternatively, the list can be summarised and plotted using \code{\link{summary.dispRity}} \code{\link{plot.dispRity}}.
#' 
#' @examples
#' data(charadriiformes)
#' 
#' ## Creating a dispRity object with a covar component
#' my_covar <-MCMCglmm.subsets(
#'                  data       = charadriiformes$data,
#'                  posteriors = charadriiformes$posteriors,
#'                  tree       = charadriiformes$tree,
#'                  group      = MCMCglmm.levels(
#'                                  charadriiformes$posteriors)[1:4],
#'                  rename.groups = c("gulls", "plovers", "sandpipers", "phylo"))
#' 
#' ## Running a projection analyses between groups (on 100 random samples)
#' between_groups <- dispRity.covar.projections(my_covar, type = "groups", base = "phylo", n = 100)
#' ## Summarising the results
#' summary(between_groups)
#' 
#' ## Measuring the projection of the elements on their own average major axis
#' elements_proj <- dispRity.covar.projections(my_covar, type = "elements", sample = mean,
#'                                             output = c("position", "distance"))
#' ## Visualising the results
#' plot(elements_proj)
#' 
#' ## Visualising the correlation
#' plot(elements_proj, specific.args = list(correlation.plot = c("position", "distance")))
#'
#' @seealso \code{\link{projections}} \code{\link{projections.between}} \code{\link{axis.covar}} \code{\link{dispRity}} \code{\link{MCMCglmm.subsets}}
#' 
#' @author Thomas Guillerme
#' @export
#' @references Guillerme T, Bright JA, Cooney CR, Hughes EC, Varley ZK, Cooper N, Beckerman AP, Thomas GH. 2023. Innovation and elaboration on the avian tree of life. Science Advances. 9(43):eadg1641.

dispRity.covar.projections <- function(data, type, base, sample, n, major.axis = 1, level = 0.95, output = c("position", "distance", "degree"), inc.base = FALSE, ..., verbose = FALSE) { #distance.method = "euclidean"

    match_call <- match.call()

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
        if(type == "elements") {
            check.class(inc.base, "logical")
        }
    }

    ## output
    check.method(output, c("position", "distance", "degree", "orthogonality"), "output must be")

    ## Check logicals
    check.class(verbose, "logical")

    ## Check optionals
    dots <- list(...)
    ## Set the defaults
    if(is.null(dots$scale)) {
        dots$scale <- TRUE
    }
    if(is.null(dots$centre)) {
        dots$centre <- TRUE
    }
    if(is.null(dots$abs)) {
        dots$abs <- TRUE
    }

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

    ## Different analyses:
    if(type == "groups") {
        ## Get the list of pairs
        if(missing(base)) {
            ## Get the pairwise list
            list_of_pairs <- unlist(apply(combn(1:length(data$subsets), 2), 2, list), recursive = FALSE)
        } else {
            if(!is(base, "list")) {
                ## Get the list paired with the id
                base_id <- which(names(data$subsets) == base)
                list_of_pairs <- sapply((1:n.subsets(data))[-base_id], function(x, base_id) c(x, base_id), base_id = base_id, simplify = FALSE)
            } else {
                ## Base is the list of pairs
                list_of_pairs <- base
            }
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
        disparity_tmp <- lapply(list_of_pairs, decompose.VCV.internal, fun = as.covar(projections.between.fast), data = data_sub, use_array = FALSE, use_tree = FALSE, measure = output, centre = dots$centre, abs = dots$abs, scale = dots$scale)
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

        ## DispRitize the results
        full_out <- list()
        for(i in 1:length(output)) {
            full_out[[i]] <- data
            full_out[[i]]$disparity <- lapply(disparity_tmp, function(X, i)return(list(elements = X[i, , drop = FALSE])), i = i)
            full_out[[i]]$call$disparity <- update_call
        }
        names(full_out) <- output
        class(full_out) <- c("dispRity", "projection")
        return(full_out)
    }

    ## B - Type groups
    if(type == "elements") {

        ## Calculating the major axes
        if(verbose) message("Calculating the major axis:...", appendLF = FALSE)
        if(missing(base)) {
            ## Which data to project
            data_proj <- data_sub
 
            ## Get the base axes
            base_axes <- axis.covar(data_proj, axis = major.axis, level = level)
        } else {
            use_base <- TRUE
            ## Get the base axis
            base_axes <- axis.covar(get.subsets(data_sub, subsets = base), axis = major.axis, level = level)
            
            ## Which data to project
            if(inc.base) {
                data_proj <- data_sub
            } else {
                data_proj <- get.subsets(data_sub, subsets = names(data_sub$subsets)[-which(names(data_sub$subsets) == base)])
            }

            ## Replicate the base axes for the mapply loop
            base_axes <- rep(base_axes, n.subsets(data_proj))
            ## Changing base axes names (for mapply output simplifications)
            names(base_axes) <- names(data_proj$subsets)
        }

        if(verbose) message("Done.")

        if(verbose) message("Calculating projections:", appendLF = FALSE)
        ## Get the groups list (in logicals)
        groups <- lapply(data_proj$subsets, function(group, data) seq_len(nrow(data$matrix[[1]])) %in% c(group$elements), data = data_proj)
   
        ## Getting all the metrics for one group
        disparity_tmp <- mapply(apply.proj, base_axes, groups, MoreArgs = list(measure = output, data = data, verbose = verbose, dots = dots), SIMPLIFY = FALSE)

        # warning("DEBUG")
        # plot(disparity_tmp[[2]][[1]][[1]], disparity_tmp[[2]][[2]][[1]]) # NO!
        # test <- apply.proj(base_axes[[2]], groups[[2]], measure = output, data = data, verbose = verbose, dots = dots)
        # plot(test[[1]][[1]], test[[2]][[1]]) # NO!

        ## Get the call updated
        update_call <- list(metrics = list())
        get.call <- function(metric) return(match.call()$metric)
        update_call$metrics$name <- get.call(as.covar(projections))
        update_call$metrics$fun <- list(as.covar(projections))
        update_call$metrics$between.groups <- FALSE

        ## Sorting all that stuff
        full_out <- list()
        for(i in 1:length(output)) {
            if(verbose) message(".", appendLF = FALSE)

            full_out[[i]] <- data
            ## Extracting the right metric
            full_out[[i]]$disparity <- lapply(disparity_tmp, function(one_subset) list(elements = do.call(cbind, lapply(one_subset, `[[`, i))))

            ## Updating the call
            full_out[[i]]$call$disparity <- update_call
        }

        if(verbose) message("Done.")
        names(full_out) <- output
        class(full_out) <- c("dispRity", "projection")
        return(full_out)
    }
}