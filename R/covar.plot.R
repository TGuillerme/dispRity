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
#' @param add logical, whether to add the plot to an existing plot (\code{TRUE}) or not (\code{FALSE}; default).
#' @param apply.to.VCV logical, if \code{ellipse} and/or \code{major.axes} is a \code{function}, whether to apply it on all the estimated ellipses/major axes (\code{FALSE}; default) or on the variance covariance matrices directly (\code{TRUE}). In other words, whether to apply the function to the ellipses/major axis or the the VCV first (e.g. the average ellipses or the ellipse of the average VCV).
#' @param ... any graphical options to be passed to \code{plot}, \code{lines} or \code{points}. See details.
#' 
#' @details
#' When specifying optional arguments with \code{...} in a graph with multiple elements (e.g. \code{points}, \code{lines}, etc...) you can specify which specific element to affect using the syntax \code{<element>.<argument>}. For example if you want everything in the plot to be in blue at the exception of the points to be red, you can use \code{covar.plot(..., col = "blue", points.col = "red")}. 
#' 
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
covar.plot <- function(data, n, points = TRUE, major.axes = FALSE, ellipses = FALSE, level = 0.95, dimensions = c(1,2), centres = colMeans, transparent.scale, add = FALSE, apply.to.VCV = FALSE, ...) {

    match_call <- match.call()
    dots <- list(...)

    ## Some sanitizing to happen in dispRity on data
    check.class(data, "dispRity")
    if(is.null(data$covar)) {
        stop.call(match_call$data, msg = "does not contain a $covar element.\nSee MCMCglmm.subsets for adding covar element to dispRity objects.")
    }

    ## Check for legend
    if(is.null(dots$legend)) {
        plot_legend <- FALSE
        dots$legend <- NULL
    } else {
        if(is.logical(dots$legend)) {
            plot_legend <-dots$legend
            dots$legend <- NULL
        }
    }

    ## Capturing the dots options
    plot_args <- c(list(x = NULL), dots)
    ## Removing specific args from dots
    remove <- c(grep(c("legend"), names(plot_args)), grep(c("lines"), names(plot_args)), grep(c("points"), names(plot_args)))
    plot_args[remove] <- NULL

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
        if(apply.to.VCV && (is(major.axes, "standardGeneric") || is(major.axes, "function")) && length(covars[[1]]) != 1) {
            ## Get the VCV central tendencies
            covars_cent_tend <- lapply(covars, VCV.cent.tend, major.axes)
            ## Get the major axis
            all_axes <- lapply(lapply(covars_cent_tend, get.one.axis, axis = 1, level = level, dimensions = dimensions), list)
        } else {
            ## The axes
            all_axes <- lapply(covars, lapply, get.one.axis, axis = 1, level = level, dimensions = dimensions)
            ## Summarising the axes (optional)
            if(is(major.axes, "standardGeneric") || is(major.axes, "function")) {
                ## Summarising the axes using the provided function
                all_axes <- lapply(all_axes, function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = major.axes)
            }
        }
    }

    ## Calculating the ellipses
    if(do_ellipses) {
        if(apply.to.VCV && (is(ellipses, "standardGeneric") || is(ellipses, "function"))) {
            ## Get the VCV central tendencies
            covars_cent_tend <- lapply(covars, VCV.cent.tend, ellipses)
            ## Get the major axis
            all_ellipses <- lapply(level.ellipses(covars_cent_tend, dimensions, npoints = 50, centres), list)
        } else {
            ## Get the ellipses
            all_ellipses <- lapply(covars, level.ellipses, dimensions, npoints = 50, centres)
            ## Summarising the ellipses (optional)
            if(is(ellipses, "standardGeneric") || is(ellipses, "function")) {
                ## Summarising the axes using the provided function
                all_ellipses <- lapply(all_ellipses, function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = ellipses)
            }
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

    ## Get the plot limits
    lims <- range(data$matrix[[1]])
    if(do_major_axes) {
        lims <- range(c(range(data$matrix[[1]]), unlist(all_axes)))
    }
    if(do_ellipses) {
        lims <- max(c(range(data$matrix[[1]]), range(unlist(all_ellipses))))
        lims <- c(-lims, lims)
    }    
    plot_args <- get.dots(plot_args, plot_args, "xlim", lims)
    plot_args <- get.dots(plot_args, plot_args, "ylim", lims)

    ## Setting the x/y labels
    percentage <- apply(data$matrix[[1]], 2, var)
    percentage <- paste0(round(percentage/sum(percentage)*100, 2), "%")
    if(!is.null(colnames(data$matrix[[1]]))) {
        column_names <- colnames(data$matrix[[1]])
    } else {
        column_names <- paste0("Dim.", 1:ncol(data$matrix[[1]]))
    }
    plot_args <- get.dots(plot_args, plot_args, "xlab", paste0(column_names[dimensions[1]], " (", percentage[dimensions[1]], ")"))
    plot_args <- get.dots(plot_args, plot_args, "ylab", paste0(column_names[dimensions[2]], " (", percentage[dimensions[2]], ")"))

    ## Plotting the background
    if(!add) {
        do.call(plot, plot_args)
    }

    ## Adding the points
    if(points) {

        ## Set up the points arguments
        points_args <- plot_args
        points_args <- get.dots(dots, points_args, "col", "black", "points")
        if(length(points_args$col) < length(data$subsets)) {
            points_args$col <- rep(points_args$col, length(data$subsets))
        }
        points_args <- get.dots(dots, points_args, "cex", 1, "points")
        if(length(points_args$cex) < length(data$subsets)) {
            points_args$cex <- rep(points_args$cex, length(data$subsets))
        }
        points_args <- get.dots(dots, points_args, "pch", 19, "points")
        if(length(points_args$pch) < length(data$subsets)) {
            points_args$pch <- rep(points_args$pch, length(data$subsets))
        }

        ## Select the groups worth plotting (i.e. ignore the global ones)
        if(length(data$subsets) > 1) {
            ## Select the groups to plot
            plot_groups <- which(size.subsets(data) != nrow(data$matrix[[1]]))
        } else {
            plot_groups <- 1:length(data$subsets)
        }
        
        ## Plot the points for each group
        for(one_group in plot_groups) {
            ## Setting the points arguments
            one_point_args <- points_args
            one_point_args$x <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[1]]
            one_point_args$y <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[2]]
            one_point_args$col <- one_point_args$col[one_group]
            one_point_args$cex <- one_point_args$cex[one_group]
            one_point_args$pch <- one_point_args$pch[one_group]
            do.call(graphics::points, one_point_args)
        }
    }

    ## Set up the lines arguments
    if(any(do_ellipses, do_major_axes)) {
        lines_args <- plot_args
        lines_args <- get.dots(dots, lines_args, "col", "black", "lines")
        if(length(lines_args$col) < length(data$subsets)) {
            lines_args$col <- rep(lines_args$col, length(data$subsets))
        }
        lines_args <- get.dots(dots, lines_args, "lty", 1, "lines")
        if(length(lines_args$cex) < length(data$subsets)) {
            lines_args$cex <- rep(lines_args$cex, length(data$subsets))
        }
        lines_args <- get.dots(dots, lines_args, "lwd", 1, "lines")
        if(length(lines_args$pch) < length(data$subsets)) {
            lines_args$pch <- rep(lines_args$pch, length(data$subsets))
        }
    }

    ## Adding the ellipses
    if(do_ellipses) {
        ## Looping through each groups' ellipse
        for(one_group in 1:length(data$subsets)) {
            one_lines_args <- lines_args
            one_lines_args$col <- adjustcolor(one_lines_args$col[one_group], alpha.f = trans_ellipses)
            #TODO: Add transparency
            lapply(all_ellipses[[one_group]], function(data, lines_args) {one_lines_args$x <- data ; do.call(lines, one_lines_args)}, one_lines_args)
        }
    }

    ## Adding the axes
    if(do_major_axes) {
        ## Looping through each groups' ellipse
        for(one_group in 1:length(data$subsets)) {
            one_lines_args <- lines_args
            one_lines_args$col <- adjustcolor(one_lines_args$col[one_group], alpha.f = trans_axes)
            #TODO: Add transparency
            lapply(all_axes[[one_group]], function(data, lines_args) {one_lines_args$x <- data ; do.call(lines, one_lines_args)}, one_lines_args)
        }
    }

    ## Add the legend
    if(plot_legend) {

        ## Set up the legend arguments
        legend_args <- plot_args
        ## Removing defaults not for legend
        legend_args[c("main", "xlim", "ylim", "xlab", "ylab", "xaxt", "yaxt")] <- NULL

        ## Get the legend arguments (plotted)
        if(!points) {
            legend_args$pch <- NULL
        } else {
            legend_args$pch <- rep(NA, length(points_args$pch))
            legend_args$pch[plot_groups] <- points_args$pch[plot_groups]
        }

        if(any(do_major_axes, do_ellipses)) {
            legend_args$lty <- lines_args$lty
            legend_args$lwd <- lines_args$lwd
        }

        ## Get the legend arguments (specific)
        legend_args <- get.dots(dots, legend_args, "col", "black")
        legend_args <- get.dots(dots, legend_args, "legend", names(data$subsets), "legend")
        legend_args <- get.dots(dots, legend_args, "x", "topleft", "legend")
        legend_args <- get.dots(dots, legend_args, "y", NULL, "legend")
        legend_args <- get.dots(dots, legend_args, "bty", "n", "legend")
        legend_args <- get.dots(dots, legend_args, "cex", 1, "legend")

        ## Add the legend
        do.call(graphics::legend, legend_args)
    }

    return(invisible())
}