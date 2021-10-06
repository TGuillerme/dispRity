#' @title covar.plot
#' @aliases sauron.plot
#'
#' @description Visualising components of a \code{dispRity} object with covar.
#'
#' @param data an \code{dispRity} object with a covar component.
#' @param n optional, a number of random posteriors to use.
#' @param points logical, whether to plot the observed elements (\code{TRUE}; default) or not (\code{FALSE}).
#' @param major.axes can be either logical for plotting all (or \code{n}) major.axes (\code{TRUE}) or none (\code{FALSE}; default) or a \code{function} for displaying one summarised major axis. See details.
#' @param ellipses can be either logical for plotting all (or \code{n}) ellipses (\code{TRUE}) or none (\code{FALSE}; default) or a \code{function} for displaying one summarised ellipse. See details. 
#' @param level the confidence interval level of the major axes and ellipses (default is \code{0.95}).
#' @param dimensions which dimensions (default is \code{c(1,2)}).
#' @param centres optional, a way to determine ellipses or major axes positions. Can be either a \code{function} (default is \code{colMeans}), a \code{vector} or a \code{list} of coordinates vectors or \code{"intercept"}. See details.
#' @param transparent.scale optional, if multiple major axes and/or ellipses are plotted, a scaling factor for the transparency. If left empty, the transparency is set to \code{1/n} or \code{0.1} (whichever is higher).
#' @param legend logical, whether to add the automatic legend (\code{TRUE}) or not (\code{FALSE}; default).
#' @param legend.args any optional argument to be passed to \code{legend}.
#' @param add logical, whether to add the plot to an existing plot (\code{TRUE}) or not (\code{FALSE}; default).
#' @param ... any graphical options to be passed to \code{plot}, \code{lines} or \code{points}.
#' 
#' @details
#' The arguments \code{major.axes} and \code{ellipses} can intake a \code{function} for summarising the display of multiple variance covariance matrices (if \code{n} is missing or greater than one). This can be any central tendency function such as \code{\link[base]{mean}}, \code{\link[stats]{median}} or \code{\link[dispRity]{mode.val}}.
#' 
#' The argument \code{centres} allows to determine how to calculate the centre of each ellipses or major axes. The argument can be either:
#' \itemize{
#'      \item A \code{function} to calculate the centre from a group like the default \code{colMeans} function that calculates the centroid coordinates of each group;
#'      \item A \code{numeric} value to be replicated as the coordinates for the centre of each group (e.g. \code{centres = 0} sets all the centres at the coordinates \code{c(0,0,0,...)}); or a vector of numeric values to be directly used as the coordinates for each group (e.g. \code{centres = c(1,2,3)} sets all the centres at the coordinates \code{c(1,2,3)}); or a list of numeric values or numeric vectors to be used as the coordinates for the centres of each group;
#'      \item code{"intercept"} for using the estimated posterior intercept for each sample.
#' }
#' 
#' \emph{NOTE} that if the input contains more dimensions than the visualised dimensions (by default \code{dimensions = c(1,2)}) the ellipses and major axes are projected from an n-dimensional space onto a 2D space which might make them look incorrect.
#' \emph{NOTE} also that the ellipses and major axes are measured independently, when summarising both parameters (e.g. by using \code{ellipses = mean} and \code{major.axes = mean}), the displayed summarised major axes is not calculated from the summarised ellipse but from the coordinates of all major axes (and therefore might not match the coordinates of the ellipse).
#' 
#' @examples
#' data(charadriiformes)
#' 
#' ## Creating a dispRity object from the charadriiformes model
#' covar <- MCMCglmm.subsets(data       = charadriiformes$data,
#'                           posteriors = charadriiformes$posteriors,
#'                           group      = MCMCglmm.levels(
#'                                          charadriiformes$posteriors)[1:4],
#'                           rename.groups = c("gulls", "plovers",
#'                                             "sandpipers", "phylogeny"))
#' 
#' ## Default plot
#' covar.plot(covar)
#' 
#' ## Same plot with more options
#' covar.plot(covar, n = 50, ellipses = mean, major.axes = TRUE, 
#'            col = c("orange", "blue", "darkgreen", "grey", "grey"),
#'            legend = TRUE, points = TRUE, cex = 0.2,
#'            main = "Charadriiformes shapespace")
#'
#' @seealso \code{\link{MCMCglmm.subsets}} \code{\link{covar.utilities}}
#' 
#' @author Thomas Guillerme
#' @export
covar.plot <- function(data, n, points = TRUE, major.axes = FALSE, ellipses = FALSE, level = 0.95, dimensions = c(1,2), centres = colMeans, transparent.scale, legend = FALSE, legend.args, add = FALSE, ...) {

    match_call <- match.call()

    ## Some sanitizing to happen in dispRity on data
    check.class(data, "dispRity")
    if(is.null(data$covar)) {
        stop.call(match_call$data, msg = "does not contain a $covar element.\nSee MCMCglmm.subsets for adding covar element to dispRity objects.")
    }

    ## Collecting the plot arguments
    plot_args <- list(x = NULL, ...)

    ## Selecting n
    if(missing(n)) {
        n <- length(data$covar[[1]])
    } else {
        check.class(n, c("numeric", "integer"))
    }
    covars <- sample.n(data$covar, n)

    ## Dimensions
    check.class(dimensions, c("integer", "numeric"))
    check.length(dimensions, 2, msg = " argument must contain only 2 dimensions (for now).")

    ## Selecting the centres
    centre_class <- check.class(centres, c("function", "numeric", "integer", "list", "character"), msg = " must be either a function (e.g. colMeans) a set or a list of sets of coordinates (e.g. list(c(1,2), c(0,0))) or \"intercept\" for using the posteriors intercepts.")
    centre_class <- ifelse(centre_class == "standardGeneric", "function", centre_class)
    centre_class <- ifelse(centre_class == "integer", "numeric", centre_class)
    
    ## Adjusting the centres to match the dimensions
    if(centre_class != "character") {
        ## Get the centres
        centres <- switch(centre_class,
            "function" = lapply(unlist(data$subsets, recursive = FALSE) ,function(group, data, fun) fun(data[c(group), ]), data = data$matrix[[1]], fun = centres),
            "numeric"  = replicate(length(data$subsets), adjust.centre(centres, data$call$dimensions), simplify = FALSE),
            "list"     = lapply(centres, adjust.centre, dim = data$call$dimension))
        names(centres) <- names(data$subsets)

        ## recentre covar matrices
        covars <- mapply(recentre, covars, centres, MoreArgs = list(dimensions = dimensions), SIMPLIFY = FALSE)
    } else {
        ## Handled by ellipses and axes
        if(centre)
        centres <- "intercept"
    }

    ## Axes and ellipses arguments
    major_axes_class <- check.class(major.axes, c("logical", "function", "standardGeneric"), msg = " must be either logical or a function for summarising the major axes.")
    do_major_axes <- !(major_axes_class == "logical" && !major.axes)
    ellipses_class <- check.class(ellipses, c("logical", "function", "standardGeneric"), msg = " must be either logical or a function for summarising the ellipses.")
    do_ellipses <- !(ellipses_class == "logical" && !ellipses)

    ## Measuring the axes
    if(do_major_axes) {
        ## The axes
        all_axes <- lapply(covars, lapply, get.one.axis, axis = 1, level = level, dimensions = dimensions)
        ## Summarising the axes (optional)
        if(is(major.axes, "standardGeneric") || is(major.axes, "function")) {
            ## Summarising the axes using the provided function
            all_axes <- lapply(all_axes, function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = major.axes)
        }
    }

    ## Calculating the ellipses
    if(do_ellipses) {
        ## Get the ellipses
        all_ellipses <- lapply(covars, level.ellipses, dimensions, npoints = 50, centres)
        ## Summarising the ellipses (optional)
        if(is(ellipses, "standardGeneric") || is(ellipses, "function")) {
            ## Summarising the axes using the provided function
            all_ellipses <- lapply(all_ellipses, function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = ellipses)
        }
    }

    ## Adjust the color
    if(missing(transparent.scale)) {
        trans_axes     <- ifelse(do_major_axes, length(all_axes[[1]]), 1)
        trans_ellipses <- ifelse(do_ellipses, length(all_ellipses[[1]]), 1)
        trans_axes <- ifelse(3/trans_axes < 0.1, 0.1, 3/trans_axes) 
        trans_ellipses <- ifelse(3/trans_ellipses < 0.1, 0.1, 3/trans_ellipses)
    } else {
        check.class(transparent.scale, "numeric")
        trans_axes <- trans_ellipses <- transparent.scale
    }

    ## Setting the plot parameters
    if(is.null(plot_args$col)) {
        plot_args$col <- "black"
    }
    if(length(plot_args$col) < length(data$subsets)) {
        plot_args$col <- rep(plot_args$col, length(data$subsets))
    }
    if(is.null(plot_args$pch)) {
        plot_args$pch <- 19
    }
    if(length(plot_args$pch) < length(data$subsets)) {
        plot_args$pch <- rep(plot_args$pch, length(data$subsets))
    }
    if(is.null(plot_args$cex)) {
        plot_args$cex <- 1
    }
    if(length(plot_args$cex) < length(data$subsets)) {
        plot_args$cex <- rep(plot_args$cex, length(data$subsets))
    }
    if(is.null(plot_args$lty)) {
        plot_args$lty <- 1
    }
    if(length(plot_args$lty) < length(data$subsets)) {
        plot_args$lty <- rep(plot_args$lty, length(data$subsets))
    }
    ## Get the plot limits
    if(is.null(plot_args$xlim)) {
        ## Default plot size
        plot_args$xlim <- range(data$matrix[[1]])
        ## Adding axes
        if(do_major_axes) {
            plot_args$xlim <- range(c(plot_args$xlim, unlist(all_axes)))
        }
        ## Adding ellipses (and preserving isometry)
        if(do_ellipses) {
            plot_args$xlim <- max(c(plot_args$xlim, range(unlist(all_ellipses))))
            plot_args$xlim <- c(-plot_args$xlim, plot_args$xlim)
        }
    }

    if(is.null(plot_args$ylim)) {
        ## Default plot size (squared)
        plot_args$ylim <- plot_args$xlim
    }

    ## Setting the x/y labels
    percentage <- apply(data$matrix[[1]], 2, var)
    percentage <- paste0(round(percentage/sum(percentage)*100, 2), "%")
    if(!is.null(colnames(data$matrix[[1]]))) {
        column_names <- colnames(data$matrix[[1]])
    } else {
        column_names <- paste0("Dim.", 1:ncol(data$matrix[[1]]))
    }
    if(is.null(plot_args$xlab)) {
        plot_args$xlab <- paste0(column_names[dimensions[1]], " (", percentage[dimensions[1]], ")")
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- paste0(column_names[dimensions[2]], " (", percentage[dimensions[2]], ")")
    }

    ## Plotting the background
    if(!add) {
        do.call(plot, plot_args)
    }

    ## Adding the points
    if(points) {
        ## Select the groups worth plotting (i.e. ignore the global ones)
        if(length(data$subsets) > 1) {
            ## Select the groups to plot
            plot_groups <- which(size.subsets(data) != nrow(data$matrix[[1]]))
            ## Update the pch in plot_args
            plot_args$pch[-plot_groups] <- NA
        } else {
            plot_groups <- 1:length(data$subsets)
        }
        
        for(one_group in plot_groups) {
            ## Setting the points arguments
            points_args <- plot_args
            points_args$x <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[1]]
            points_args$y <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[2]]
            points_args$col <- plot_args$col[one_group]
            points_args$cex <- plot_args$cex[one_group]
            points_args$pch <- plot_args$pch[one_group]
            do.call(graphics::points, points_args)
        }
    }

    ## Adding the ellipses
    if(do_ellipses) {
        ## Add the ellipses
        line_args <- plot_args
        ## Looping through the groups
        for(one_group in 1:length(data$subsets)) {
            line_args$col <- adjustcolor(plot_args$col[one_group], alpha.f = trans_ellipses)
            #TODO: Add transparency
            lapply(all_ellipses[[one_group]], function(data, line_args) {line_args$x <- data ; do.call(lines, line_args)}, line_args)
        }
    }

    ## Adding the axes
    if(do_major_axes) {
        ## Plot the axes
        line_args <- plot_args
        for(one_group in 1:length(data$subsets)) {
            line_args$col <- adjustcolor(plot_args$col[one_group], alpha.f = trans_axes)
            #TODO: Add transparency
            lapply(all_axes[[one_group]], function(data, line_args) {line_args$x <- data ; do.call(lines, line_args)}, line_args)
        }
    }

    ## Add the legend
    if(legend){

        ## Missing list
        if(missing(legend.args)) {
            legend.args <- list()
        }
        ## Names
        if(is.null(legend.args$legend)) {
            legend.args$legend <- names(data$subsets)
        }
        ## Legend pch
        if(is.null(legend.args$pch)) {
            if(!points) {
                legend.args$pch <- NULL
            } else {
                legend.args$pch <- rep(NA, length(plot_args$pch))
                legend.args$pch[plot_groups] <- plot_args$pch[plot_groups]
            }
        }
        ## Legend position
        if(is.null(legend.args$x)) {
            legend.args$x <- "topleft"
        }
        ## Legend box
        if(is.null(legend.args$bty)) {
            legend.args$bty <- "n"
        }
        ## Legend col
        if(is.null(legend.args$col)) {
            legend.args$col <- plot_args$col
        }
        ## Legend lty
        if(is.null(legend.args$lty)) {
            legend.args$lty <- plot_args$lty
        }

        ## Add the legend
        do.call(graphics::legend, legend.args)
    }

    return(invisible())
}