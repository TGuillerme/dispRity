## Get the disparity characteristics
get.data.params <- function(data) {
    return(list(
        "distribution"   = ifelse(length(data$disparity[[1]]$elements) != 1, TRUE, FALSE),
        "bootstrap"      = ifelse(!is.null(data$call$bootstrap), TRUE, FALSE),
        "rarefaction"    = data$call$bootstrap[[3]],
        "between.groups" = data$call$disparity$metrics$between.groups,
        "elements"       = names(data$disparity)
        ))
}

## Setting up all the default plot parameters:
# $disparity: summarised_data
# $helpers: n_quantiles, npoints
# $options: ylim, ylab, xlab, col, ...
# $observed: data, ylim, ylab, xlab, col, ...
# $elements: elements, ylim, ylab, xlab, col, ...
get.plot.params <- function(data, data_params, cent.tend, quantiles, rarefaction_level, elements_args, type, observed_args, ...) {

    ## Set up the plotting data
    ## Summarise the data
    if(is.null(data_params$model.sim)) {
        # if((!is.na(data$call$subsets["trees"])) && (as.numeric(data$call$subsets["trees"]) > 1)) {
        #     summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5, na.rm = TRUE)
        # } else {
            summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5, na.rm = TRUE)
        # }
    } else {
        summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5)
        ## Making a fake obs column
        colnames(summarised_data)[3] <- "obs"
    }

    ## Find the observed data rows (that are not rarefactions)
    observed_data <- !is.na(summarised_data[,(2+ifelse(data_params$between.groups, 2, 1))])
    if(any(!observed_data)) {
        ## Find NAs in the last column
        last_col_na <- is.na(summarised_data[, ncol(summarised_data)])
        ## If there are any NA in the last column, the observed NA is a true NA (nor rarefaction)
        observed_data <- last_col_na | observed_data
    }

    ## Get a specific rarefaction level
    if(!is.null(rarefaction_level)) {
        ## Rarefaction level
        if(data_params$between.groups) {
            ## Find the matching rarefaction level
            rarefaction_match <- which(summarised_data$n_1 == rarefaction_level & summarised_data$n_2 == rarefaction_level)
        } else {
            rarefaction_match <- which(summarised_data$n == rarefaction_level)
        }
        ## Get both the observed and rarefied data
        selected_data <- summarised_data[rarefaction_match ,]
    } else {
        ## Just get the observed data
        selected_data <- summarised_data[observed_data,]
    }

    ## Separate the data elements
    disparity <- list()
    name_part <- c(1:ifelse(data_params$between.groups, 3, 2))
    disparity$names <- selected_data[, name_part, drop = FALSE]
    disparity$data  <- selected_data[, -name_part, drop = FALSE]

    ## Update the data to be in boxplot format
    if(type == "box") {
        ## Extracting boxplot data
        if(is.null(rarefaction_level)) {
            ## Getting the observed or bootstrapped data
            if(data_params$bootstrap && !observed_args$observed) {
                ## Getting the bootstrapped data
                box_data <- do.call(cbind, unlist(extract.dispRity(data, observed = FALSE), recursive = FALSE))
            } else {
                if(data_params$distribution || data_params$between.groups) {
                    box_data <- do.call(cbind, extract.dispRity(data, observed = TRUE))
                } else {
                    box_data <- do.call(cbind, unlist(extract.dispRity(data, observed = FALSE), recursive = FALSE))
                }
            }
        } else {
            ## Find the correct rarefaction level
            box_data <- do.call(cbind, unlist(extract.dispRity(data, observed = FALSE, rarefaction = rarefaction_level), recursive = FALSE))
        }
        ## Updating the disparity data part
        if(data_params$between.groups) {
            colnames(box_data) <- data_params$elements
        }
        disparity$data <- box_data
    }

    ## Set up the helpers options
    helpers <- list()
    ## Detect the number of quantiles
    helpers$n_quantiles <- length(quantiles)
    ## Detect the number of points
    if(is.null(data_params$model.sim)) {
        helpers$n_points <- length(data$disparity)
    } else {
        ## Selecting helpers for model.sim
        helpers$n_points <- nrow(disparity$data)
    }

    ## Set up the plotting options
    dots <- list(...)
    options <- list()

    ## Set the xlabel
    if(is.null(dots$xlab)) {
        ## Default is subsets
        options$xlab <- "Subsets"
        ## Default chrono.subset label
        if(type == "continuous" && "continuous" %in% data$call$subsets) {
            options$xlab <- "Time (Mya)"
        } 
        ## Default rarefaction label
        if(type == "rarefaction") {
            options$xlab <- "Elements"
        }
    } else {
        ## User input
        check.class(dots$xlab, "character", " must be a character string.")
        check.length(dots$xlab, 1, " must be a character string.")
        options$xlab <- dots$xlab
        dots$xlab <- NULL
    }

    ## Set the ylabel
    if(is.null(dots$ylab)) {
        ## Default is the metric name
        options$ylab <- as.character(data$call$disparity$metrics$name)
    } else {
        ## User input        
        check.class(dots$ylab, "character", " must be a character string.")
        if(length(dots$ylab) > 2) stop.call("", "ylab can have maximum of two elements.")
        options$ylab <- dots$ylab
        dots$ylab <- NULL
    }

    ## Set the y limits
    if(is.null(dots$ylim)) {
        if(type != "rarefaction") {
            ## Get the range of the data
            options$ylim <- range(disparity$data, na.rm = TRUE)
            ## Add 2% on each side
            percent_change <- 0.02
            if(options$ylim[1] > 0) {
                options$ylim[1] <- options$ylim[1] - options$ylim[1]*percent_change
            } else {
                options$ylim[1] <- options$ylim[1] + options$ylim[1]*percent_change
            }
            options$ylim[2] <- options$ylim[2] + options$ylim[2]*percent_change
        } else {
            options$ylim <- "rarefaction"
        }
    } else {
        ## User input
        check.class(dots$ylim, c("numeric", "integer"))
        check.length(dots$ylim, 2, " must be a vector of two elements.")
        options$ylim <- dots$ylim
        dots$ylim <- NULL
    }

    ## Set the colours
    if(is.null(dots$col)) {
        ## For boxplots, the default colour is white
        if(type == "box" && type != "rarefaction") {
            options$col <- "white"
        } else {
            options$col <- "black"
        }
    } else {
        ## User input
        check.class(dots$col, "character", " must be a character string.")
        options$col <- dots$col
        dots$col <- NULL
    }
    ## Add potential missing colours
    if(helpers$n_quantiles > 0 && type != "box") {
        ## Count if they are any missing colours
        cols_missing <- (helpers$n_quantiles + 1) - length(options$col)
        ## Adding grey scales if quantiles
        if(cols_missing > 0) {
            colfun <- grDevices::colorRampPalette(c("grey", "lightgrey"))
            options$col <- c(options$col, colfun(cols_missing))
        }
    }

    ## Add additional options
    if(length(dots) > 0) {
        options <- c(options, dots)    
    }
    
    ## Observed data
    if(observed_args$observed) {
        ## Adding the observed data
        selected_data <- summarised_data[observed_data,]
        observed_args$names <- selected_data[, name_part]
        observed_args$data  <- selected_data[, -name_part]

        ## Default observed arguments
        if(is.null(observed_args$col)) {
            if(type == "box" && options$col[[1]] == "white") {
                observed_args$col <- "black"
            } else {
                observed_args$col <- options$col[[1]]
            }
        }
        if(is.null(observed_args$pch)) {
            observed_args$pch <- 4
        }
        if(is.null(observed_args$cex)) {
            observed_args$cex <- 1
        }
    }

    ## Observed data
    if(elements_args$elements) {
        ## Default observed arguments
        if(is.null(elements_args$col)) {
            if(type == "box" && options$col[[1]] == "white") {
                elements_args$col <- c("black", "darkgrey")
            } else {
                elements_args$col <- options$col
            }
        }
        if(length(elements_args$col) < 2) {
            elements_args$col <- rep(elements_args$col, 2)
        }

        if(is.null(elements_args$pch)) {
            elements_args$pch <- c(15, 15)
        } else {
            if(length(elements_args$pch) < 2) {
                elements_args$pch <- rep(elements_args$pch, 2)
            }
        }
        if(is.null(elements_args$lty)) {
            elements_args$lty <- c(2, 2)
        } else {
            if(length(elements_args$lty) < 2) {
                elements_args$lty <- rep(elements_args$lty, 2)
            }            
        }
    }

    ## Output the finalised list
    return(list("disparity" = disparity, "helpers" = helpers, "options" = options, "observed_args" = observed_args, "elements_args" = elements_args))
}

## Handle the shift argument
get.shift <- function(add, plot_params) {
    ## Set the shift parameter (for add)
    shift = 0
    if(add) {
        ## Is the previous plot the same size?
        prev_axis <- par("xaxp")
        if(prev_axis[2] == plot_params$helpers$n_points) {
            shift = 0
        } else {
            shift = 0.5
        }
    }
    return(shift)    
}

## Getting the columns for the right quantiles
get.quantile.col <- function(cent_tend_col, cis, n_quantiles) {
    return(c(
        ## Lower quantile
        cent_tend_col + cis,
        ## Higher quantile
        (cent_tend_col + n_quantiles*2) - (cis-1)
        ))
}

## Observed points
plot.observed <- function(plot_params) {
    if(plot_params$observed_args$observed) {

        ## Set the observed arguments
        points_args <- plot_params$observed_args
        points_args$x <- 1:plot_params$helpers$n_points
        points_args$y <- points_args$data[, "obs"]
        points_args$observed <- NULL
        points_args$names <- NULL
        points_args$data <- NULL

        ## Calling the points
        do.call(points, points_args)
    } else {
        return(invisible())
    }
}

## Plot elements
plot.elements <- function(plot_params, data_params, type) {
    ## Elements plots
    add_args <- plot_params$elements_args
    add_args$elements <- NULL

    ## Getting the scaling value
    percent_change <- 0.02
    rescaling <- plot_params$options$ylim
    if(rescaling[1] >= 0) {
        rescaling[1] <- rescaling[1] + percent_change * rescaling[1]
    } else {
        rescaling[1] <- rescaling[1] - percent_change * rescaling[1]
    }
    rescaling[2] <- rescaling[2] - percent_change * rescaling[2]

    ## Rescale the y values
    rescaled_elements <- plot_params$disparity$names[,-1, drop = FALSE]
    rescaled_elements <- matrix(scales::rescale(unlist(rescaled_elements), to = rescaling), ncol = ncol(rescaled_elements), byrow = FALSE)

    ## Add the values
    add_args$x <- 1:plot_params$helpers$n_points
    add_args$y <- rescaled_elements[,1]

    ## Select the first colour
    add_args$col <- plot_params$elements_args$col[1]

    ## Check if ylab exists
    if(is.null(plot_params$elements_args$ylab)) {
        axis_label <- ifelse(length(plot_params$options$ylab) > 1, plot_params$options$ylab[2], "Elements")
    } else {
        axis_label <- plot_params$elements_args$ylab
    }

    ## Add the different types
    if(type == "continuous") {
        add_args$lty <- plot_params$elements_args$lty[1]
        do.call(lines, add_args)
    } else {
        add_args$pch <- plot_params$elements_args$pch[1]
        do.call(points, add_args)
    } 

    ## Add the ones for the second group (if two groups)
    if(data_params$between.groups) {
        ## Get the n_2 column
        add_args$y <- rescaled_elements[,2]
        ## Select the second colour
        add_args$col <- plot_params$elements_args$col[2]
        
        ## Add the different types
        if(type == "continuous") {
            add_args$lty <- plot_params$elements_args$lty[2]
            do.call(lines, add_args)
        } else {
            ## Slightly shift
            add_args$x <- add_args$x + (mode.val(diff(add_args$x))*0.01)
            add_args$pch <- plot_params$elements_args$pch[2]
            do.call(points, add_args)
        } 
    }    

    ## Add the axis label
    seq.along.range <- function(range, out = 5) {
        seq(from = range(range)[1],
            to = range(range)[2],
            length.out = out)
    }
    
    ## Add the second y axis
    axis(4, at = seq.along.range(rescaling), labels = seq.along.range(plot_params$disparity$names[,-1]),
        lty = plot_params$elements_args$lty)
    ## Add the second axis label
    mtext(axis_label, side = 4, line = 2)
}

## discrete plotting
plot.discrete <- function(plot_params, data_params, add, density, type) {

    ## Get the shifting argument
    shift <- get.shift(add, plot_params)

    ## Select the central tendency column
    cent_tend_col <- ifelse(data_params$bootstrap, 2, 1)

    ## Creating the matrix frame (for neat plotting)
    frame_matrix <- matrix(1:plot_params$helpers$n_points,
                           ncol = plot_params$helpers$n_point,
                           dimnames = list(c(), plot_params$disparity$names[,1]))

    if(!add) {
        ## Set the frame boxplot arguments
        box_args <- plot_params$options
        box_args$x <- frame_matrix
        box_args$col <- box_args$border <- "white"
        box_args$ylab <- box_args$ylab[[1]]
        box_args$boxwex <- 0.00001
        box_args$type <- "n"
        ## Plot the boxplot
        do.call(boxplot, box_args)
    }

    ## Check if bootstrapped
    if(data_params$bootstrap || data_params$distribution) {
        ## Set the width (default)
        width <- 0.5 

        ## Initialise the plot arguments
        plot_args <- plot_params$options

        ## Add the quantiles
        if(type == "polygon") {

            ## Add the polygon options
            plot_args$border <- plot_params$options$col[[1]]
            plot_args$density <- density

            ## Loop through each point
            for(point in 1:plot_params$helpers$n_points) {
                ## Loop through each quantile
                for(cis in 1:plot_params$helpers$n_quantiles) {
                    ## Add the colour option
                    plot_args$col <- plot_params$options$col[[cis+1]]
                    ## Set the bars width
                    point_width <- width/(plot_params$helpers$n_quantiles - cis + 1.5)
                    ## Set the x coordinates
                    plot_args$x <- c(point - point_width,
                                     point + point_width,
                                     point + point_width,
                                     point - point_width) + shift
                    ## Select the quantiles columns
                    quantiles_col <- get.quantile.col(cent_tend_col, cis, plot_params$helpers$n_quantiles)
                    ## Set the y coordinates
                    plot_args$y <- c(rep(plot_params$disparity$data[point, quantiles_col[1]], 2),
                                     rep(plot_params$disparity$data[point, quantiles_col[2]], 2))

                    ## Add the polygon
                    do.call(polygon, plot_args)
                }
            }
        }

        if(type == "line") {
            ## Loop through each point
            for(point in 1:plot_params$helpers$n_points) {
                ## Loop through the quantiles
                for(cis in 1:plot_params$helpers$n_quantiles) {
                    ## The the line type
                    plot_args$lty <- plot_params$helpers$n_quantiles - cis + 1
                    plot_args$lwd <- 1 + cis * 1.5
                    plot_args$col <- plot_params$options$col[[cis+1]]
                    ## Set the x values
                    plot_args$x <- rep(point, 2)
                    ## Select the quantiles columns
                    quantiles_col <- get.quantile.col(cent_tend_col, cis, plot_params$helpers$n_quantiles)
                    ## Set the y value
                    plot_args$y <- c(plot_params$disparity$data[point, quantiles_col[1]],
                                     plot_params$disparity$data[point, quantiles_col[2]])

                    ## Plotting the line
                    do.call(lines, plot_args)
                }
            }
        }
    } 
    ## Add the points estimates
    point_args <- list()
    ## Get the points coordinates
    point_args$x <- 1:plot_params$helpers$n_points + shift
    point_args$y <- plot_params$disparity$data[, cent_tend_col]
    ## Get the options
    point_args$col <- plot_params$options$col[[1]]
    if(is.null(plot_params$options$pch)) {
        point_args$pch <- 19
    } else {
        point_args$pch <- plot_params$options$pch
    }

    ## Add the points
    do.call(points, point_args)

    ## Save parameters
    return(par())
}

## continuous plotting
plot.continuous <- function(plot_params, data_params, add, density) {
    ## Get the shifting argument
    shift <- get.shift(add, plot_params)

    ## Select the central tendency column
    cent_tend_col <- ifelse(data_params$bootstrap, 2, 1)

    ## Set up the cur plot options
    plot_args <- plot_params$options
    plot_args$x <- seq(from = 1, to = plot_params$helpers$n_points)-shift
    plot_args$y <- plot_params$disparity$data[, cent_tend_col]
    plot_args$type <- "l"
    plot_args$col <- plot_params$options$col[[1]]
    plot_args$xaxt <- "n"

    ## Plot the central tendency
    if(!add) {
        ## Plot the thingy
        do.call(plot, plot_args)

        ## Add the axis labels
        options(warn = -1)
        try_round <- as.numeric(plot_params$disparity$names[,"subsets"])
        options(warn = 0)

        if(all(is.na(try_round))) {
            ## Keep the ticks as they are
            x_ticks <- plot_params$disparity$names[,"subsets"]
        } else {
            rounding <- 3-max(nchar(round(try_round)))
            x_ticks <- round(try_round, digits = ifelse(rounding < 0, 0, rounding))
        }
        axis(1, 1:plot_params$helpers$n_points, x_ticks)
    } else {
        ## Plot the line
        do.call(lines, plot_args)
    }

    ## Check if bootstrapped
    if(data_params$bootstrap || data_params$distribution) {

        ## Setting the plot parameters
        poly_args <- plot_params$options
        poly_args$border <- "NA"
        poly_args$density <- density
        poly_args$x <- numeric()
        poly_args$y <- numeric()

        ## Add the polygons
        for (cis in 1:plot_params$helpers$n_quantiles) {

            ## Set the x values
            poly_args$x <- c(1:plot_params$helpers$n_points)
            poly_args$x <- c(poly_args$x, rev(poly_args$x))
            ## Select the quantiles columns
            quantiles_col <- get.quantile.col(cent_tend_col, cis, plot_params$helpers$n_quantiles)
            ## Set the y values
            poly_args$y <- plot_params$disparity$data[, quantiles_col[1]]
            poly_args$y <- c(poly_args$y, rev(plot_params$disparity$data[, quantiles_col[2]]))
            ## Set up the colour
            poly_args$col <- plot_params$options$col[cis+1]

            ## Dividing the polygon if NAs
            if(any(is_nas <- is.na(poly_args$y[1:plot_params$helpers$n_points]))) {

                ## Selecting the groups of applicable data
                groups <- numeric()
                group_label <- 1
                ## (attributing a group as soon as it jumps an NA)
                for(point in seq_along(is_nas)) {
                    if(is_nas[point]) {
                        ## Increment the group
                        group_label <- group_label + 1
                        groups[point] <- NA
                    } else {
                        groups[point] <- group_label
                    }
                }

                ## splitting the data into the groups
                split.combine.data <- function(var, groups, n_points) {
                    vals_1 <- split(var[1:n_points], groups)
                    vals_2 <- split(var[-c(1:n_points)], rev(groups))
                    return(mapply(c, vals_1, vals_2, SIMPLIFY = FALSE))
                }
                y_vals <- split.combine.data(poly_args$y, groups, plot_params$helpers$n_points)
                x_vals <- split.combine.data(poly_args$x, groups, plot_params$helpers$n_points)

                ## Combine args
                combined.poly.args <- function(x, y, poly_args) {
                    poly_args$x <- x
                    poly_args$y <- y
                    return(poly_args)
                }
                list_poly_args <- mapply(combined.poly.args, x_vals, y_vals, SIMPLIFY = FALSE, MoreArgs = list("poly_args" = poly_args))

                ## Plot all the polygons
                lapply(list_poly_args, function(args) do.call(polygon, args))
            } else {
                ## Plot the polygon
                do.call(polygon, poly_args)
            }
        }

        ## Add the central tendency
        do.call(lines, plot_args)
    }

    ##  Save parameters
    return(par())
}

## Plot the rarefaction
plot.rarefaction <- function(plot_params, data_params, data) {

    ## How many rarefaction plots?
    n_plots <- length(data$subsets)

    ## Open the multiple plots
    plot_size <- ifelse(n_plots == 3, 4, n_plots)
    op_tmp <- par(mfrow = c(ceiling(sqrt(plot_size)),round(sqrt(plot_size))))

    ## Get the list of subsets
    subsets_levels <- names(data$subsets)

    ## Get all the rarefaction values
    rarefied_data <- summary(data)

    ## Get were the central tendency value is
    cent_tend_col <- ifelse(data_params$between.groups, 5, 4)

    ## Setting the plotting arguments
    all_plot_args <- plot_params$options
    all_plot_args$ylim <- NULL
    all_plot_args$lty <- 1
    all_plot_args$type <- "l"

    ## Plot the different curves
    for(one_subset in subsets_levels) {

        ## get the subset data
        subset_data <- rarefied_data[rarefied_data[,1] == one_subset, ]

        ## Setting the plotting args for the specific subset
        one_plot_args <- all_plot_args
        one_plot_args$x <- subset_data[,2]
        one_plot_args$y <- subset_data[,cent_tend_col]
        one_plot_args$ylim <- range(subset_data[, -c(1:(cent_tend_col-1))])
        one_plot_args$main <- one_subset

        ## Plot the central tendency
        do.call(plot, one_plot_args)

        ## Add the quantiles
        for(cis in 1:plot_params$helpers$n_quantiles) {

            ## Get the quantile columns
            ci_cols <- get.quantile.col(cent_tend_col, cis, plot_params$helpers$n_quantiles)

            ## Set the plotting arguments
            lines_args <- one_plot_args
            lines_args$col <- all_plot_args$col[length(all_plot_args$col) - (cis-1)]
            lines_args$y <- subset_data[,ci_cols[1]]

            ## plot the quantiles
            do.call(lines, lines_args)
            lines_args$y <- subset_data[,ci_cols[2]]
            do.call(lines, lines_args)
        }
    }

    ## Done!
    par(op_tmp)
}

## Plotting a space preview
plot.preview <- function(data, specific.args, ...) {

    ## The "ggplot" colours
    gg.color.hue <- function(n) {
        grDevices::hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
    }
    make.transparent <- function(colour, levels) {
        grDevices::adjustcolor(colour, alpha.f = (1/levels) + 1/3)
    }

    ## Set up the specific args
    if(is.null(specific.args$matrix)) {
        specific.args$matrix <- c(1:length(data$matrix))
    }
    if(is.null(specific.args$dimensions)) {
        specific.args$dimensions <- c(1,2)
    }
    if(is.null(specific.args$legend)) {
        plot_legend <- TRUE
        legend_args <- list()
    } else {
        if(is(specific.args$legend, "logical")) {
            plot_legend <- specific.args$legend
            legend_args <- list()
        } else {
            plot_legend <- TRUE
            legend_args <- specific.args$legend
        }
    }
    if(is.null(specific.args$tree) || is.null(data$tree[[1]])) {
        ## Don't plot trees
        plot_trees <- FALSE
    } else {
        if(is(specific.args$tree, "logical")) {
            ## Toggle plot_trees
            plot_trees <- specific.args$tree
            if(plot_trees) {
                ## Select the trees to plot
                specific.args$tree <- 1:length(data$tree)
            }
        } else {
            ## Specific args must be the specific trees to plot
            plot_trees <- TRUE
        }
    }

    ## Capturing the dots options
    plot_args <- list(x = NULL, y = NULL, ...)

    ## Getting the loadings
    loading <- apply(do.call(rbind, lapply(data$matrix[specific.args$matrix], function(matrix) apply(matrix, 2, var, na.rm = TRUE))), 2, mean)
    loading <- round(loading/sum(loading)*100, 2)

    ## Setting the labels
    if(is.null(plot_args$xlab)) {
        plot_args$xlab <- paste0("Dimension ", specific.args$dimensions[1], " (", loading[specific.args$dimensions[1]], "%)")
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- paste0("Dimension ", specific.args$dimensions[2], " (", loading[specific.args$dimensions[2]], "%)")
    }

    ## Setting plot limits
    plot_lim <- range(unlist(lapply(data$matrix[specific.args$matrix], function(matrix, dim) c(matrix[, dim]), dim = specific.args$dimensions)))
    if(is.null(plot_args$xlim)) {
        plot_args$xlim <- plot_lim
    }
    if(is.null(plot_args$ylim)) {
        plot_args$ylim <- plot_lim
    }

    ## Get the number of colour groups
    n_groups <- length(data$subsets)
    n_groups <- ifelse(n_groups == 0, 1, n_groups)

    ## Setting the colours
    if(is.null(plot_args$col)) {
        if(n_groups == 1) {
            plot_args$col <- "black"
        } else {
            if(data$call$subsets[[1]] == "customised") {
                plot_args$col <- gg.color.hue(n_groups)
            } else {
                plot_args$col <- grDevices::heat.colors(n_groups+2)[1:n_groups]
            }
        }
    }

    ## Setting the pch
    if(is.null(plot_args$pch)) {
        plot_args$pch <- 19
    }
    if(length(plot_args$pch) != n_groups) {
        plot_args$pch <- rep(plot_args$pch, n_groups)
    }

    ## Make a colour and pch classifier
    col_order <- plot_args$col
    pch_order <- plot_args$pch
    if(n_groups > 1) {
        ## Get the list of subsets
        subsets <- unlist(data$subsets, recursive = FALSE)
        ## Make an empty classifier
        classifier <- rep(NA, nrow(data$matrix[[1]]))
        for(class in 1:n_groups) {
            ## Store the selected subsets in the classifier (potentially overriding)
            if(dim(subsets[[1]])[2] == 1) {
                classifier[c(subsets[[class]])] <- class 
            } else {
                classifier[c(elements.sampler(subsets[[class]]))] <- class
            }
        }
        plot_args$col <- plot_args$col[classifier]
        plot_args$pch <- plot_args$pch[classifier]
    }

    ## Plot the empty plot
    do.call(plot, plot_args)

    ## Plot the trees
    if(plot_trees) {
        ## Plotting one edge
        plot.edge <- function(one_edge, points_data, params) {
            params$x <- points_data[one_edge, 1] 
            params$y <- points_data[one_edge, 2] 
            do.call(lines, params)
        }

        ## Get the lines arguments
        lines_args <- plot_args
        lines_args$col <- "grey"
        if(length(specific.args$tree) > 1) {
            lines_args$col <- make.transparent(lines_args$col, levels = length(specific.args$tree))
        }
        if(is.null(lines_args$lwd)) {
            lines_args$lwd <- 1
        }
        if(is.null(lines_args$lty)) {
            lines_args$lty <- 1
        }

        ## Plotting each tree
        for(tree in specific.args$tree) {

            ## Select the right data
            if(length(data$matrix) == length(specific.args$tree)) {
                data_matrix <- data$matrix[[tree]]
            } else {
                data_matrix <- data$matrix[[1]]
            }

            ## Selecting the origin points for the tree
            points_data <- data_matrix[, specific.args$dimensions][c(data$tree[[tree]]$tip.label, data$tree[[tree]]$node.label), ] 
            ## Plotting all the edges
            apply(data$tree[[tree]]$edge, 1, plot.edge,
                  points_data = points_data,
                  params = lines_args)
        }
    }

    ## Plot the points
    for(matrix in specific.args$matrix) {
        point_args <- plot_args
        ## Transparentize the colour
        if(length(specific.args$matrix) > 1) {
            point_args$col <- make.transparent(point_args$col, levels = length(specific.args$matrix)*2)
        }
        ## Add the points per matrix
        point_args$x <- data$matrix[[specific.args$matrix[matrix]]][, specific.args$dimensions[1]]
        point_args$y <- data$matrix[[specific.args$matrix[matrix]]][, specific.args$dimensions[2]]
        ## Call the points
        do.call(points, point_args)
    }

    ## Plot the legend
    if(n_groups > 1 && plot_legend) {

        ## Set the legend arguments
        if(is.null(legend_args$x)) {
            legend_args$x <- "topright"
        }
        if(is.null(legend_args$legend)) {
            legend_args$legend <- names(data$subsets)
        }
        if(is.null(legend_args$col)) {
            legend_args$col <- col_order
        }
        if(is.null(legend_args$pch)) {
            legend_args$pch <- pch_order
        }
        if(is.null(legend_args$cex)) {
            legend_args$cex <- 0.666
        }
        ## Add the legend
        do.call(legend, legend_args)
    }

    ## Return invisible
    return(invisible())
}

## The following is a modified version of plot.randtest from ade4 v1.4-3
plot.randtest <- function(data_sub, ...) {
    plot_args <- list(...)

    ## Extracting the specific args
    specific_args <- plot_args$specific.args
    plot_args$specific.args <- NULL

    ## Add the histogram data
    plot_args$x <- data_sub$plot$hist

    ## Plot arguments
    if(is.null(plot_args$xlim)) {
        plot_args$xlim <- data_sub$plot$xlim
    }
    if(is.null(plot_args$ylim)) {
        plot_args$ylim <- c(0, max(data_sub$plot$hist$count))
    }
    if(is.null(plot_args$col)) {
        plot_args$col <- "grey"
    }

    ## Plotting the simulated data
    do.call(plot, plot_args)

    ## Observed data
    observed <- data_sub$obs

    ## Adding the observed data
    lines(c(observed, observed), c(plot_args$ylim[2]/2, 0))
    points(observed, plot_args$ylim[2]/2, pch = 18, cex = 2)

    ## Setting the specific args for the legend
    if(is.null(specific_args$legend)) {
        plot_legend <- TRUE
    } else {
        if(is(specific_args$legend, "logical")) {
            plot_legend <- specific_args$legend
        } else {
            plot_legend <- TRUE
        }
    }

    ## Adding the legend (test results)
    if(plot_legend) {
        ## Initialise the legend arguments
        if(is.null(specific_args)) {
            legend_args <- list()    
        } else {
            legend_args <- specific_args$legend
        }
        
        ## Set the default legend arguments
        if(is.null(legend_args$x)) {
            legend_args$x <- "topleft"
        }
        if(is.null(legend_args$bty)) {
            legend_args$bty <- "n"
        }
        if(is.null(legend_args$legend)) {
            legend_args$legend <- c("p-value", round(data_sub$pvalue, 5))
        }
        if(is.null(legend_args$cex)) {
            legend_args$cex <- 0.7
        }
        if(is.null(legend_args$adj)) {
            legend_args$adj <- 0.2
        }
        do.call(legend, legend_args)
    }
}

## The following is a modified version of dtt plots (from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R)
plot.dtt <- function(data, quantiles, cent.tend, density, ...) {
    plot_args <- list(...)

    ## Set the default options
    if(is.null(plot_args$ylim)) {
        ## The base y-limit
        plot_args$ylim <- c(range(pretty(data$dtt)))

        ## Add the simulation (if exist)
        if(!is.null(data$sim)) {
            plot_args$ylim <- range(c(plot_args$ylim , range(data$sim)))
        }
    }
    if(is.null(plot_args$xlab)) {
        plot_args$xlab <- "scaled time"
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- paste0("scaled ", as.character(data$call[[3]]))
    }
    if(is.null(plot_args$col)) {
        colfun <- grDevices::colorRampPalette(c("lightgrey", "grey"))
        plot_args$col <- c("black", colfun(length(quantiles)))
    }

    ## Add the data for the plot args
    plot_args$x <- data$times
    plot_args$y <- data$dtt
    plot_args$type <- "n"

    ## Empty plot
    do.call(plot, plot_args)

    ## Add the simulated data
    if(!is.null(data$sim)) {

        ## Check the quantiles
        check.class(quantiles, "numeric", " must be any value between 1 and 100.")
        ## Are quantiles probabilities or proportions ?
        if(any(quantiles < 1)) {
            ## Transform into proportion
            quantiles <- quantiles*100
        }
        ## Are quantiles proper proportions
        if(any(quantiles < 0) | any(quantiles > 100)) {
            stop.call("", "quantiles(s) must be any value between 0 and 100.")
        }
        n_quantiles <- length(quantiles)

        ## Check the central tendency
        check.class(cent.tend, "function")
        ## The function must work
        if(make.metric(cent.tend, silent = TRUE)$type != "level1") {
            stop.call("", "cent.tend argument must be a function that outputs a single numeric value.")
        }

        ## Summarised data
        quantiles_values <- apply(data$sim, 1, quantile, probs = CI.converter(quantiles), na.rm = TRUE)
        cent_tend_values <- apply(data$sim, 1, cent.tend)

        ## Plotting the polygons for each quantile
        for(cis in 1:n_quantiles) {
            xx <- c(data$times, rev(data$times))
            yy <- c(quantiles_values[(n_quantiles*2) - (cis-1), ], rev(quantiles_values[cis ,]))
            polygon(xx, yy, col = plot_args$col[cis+1], border = FALSE, density = density)
        }

        ## Add the central tendency
        lines(data$times, cent_tend_values, col = plot_args$col[1], lty = 2)
    }

    ## Add the observed disparity
    line_args <- plot_args
    line_args$type <- NULL
    line_args$col <- plot_args$col[1]
    if(is.null(plot_args$lwd)) {
        line_args$lwd <- 1.5
    }
    do.call(lines, line_args)
}

## Plotting model tests results
plot.model.test <- function(data, ...) {

    plot_args <- list(...)

    ## Set the default plotting arguments
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- "weighted AIC"
    }
    if(is.null(plot_args$col)) {
        plot_args$col <- "grey"
    }

    ## Extracting the weighted aicc
    aic_values <- data$aic.models[, 3]

    ## Ordering the weighted aicc
    plot_args$height <- aic_values[order(aic_values, decreasing = TRUE)]

    ## Plot
    do.call(barplot, plot_args)
}

## Plotting the model simulation results
plot.model.sim <- function(data, add, density, quantiles, cent.tend, ...) {

    dots <- list(...)

    ## Get inherited subsets (always from simulations to avoid NAs)
    subset_names <- rev(data$simulation.data$fix$subsets)

    ## Set up the data parameters
    data_params <- list("distribution"   = TRUE,
                        "bootstrap"      = TRUE,
                        "rarefaction"    = NULL,
                        "between.groups" = FALSE,
                        "elements"       = subset_names,
                        "model.sim"      = TRUE)
    ## Model names
    if(is.null(dots$main)) {
        ## Get the title
        if(is(data$model, "character")) {
            plot_main <- data$model
        } else {
            plot_main <- paste0(rownames(data$model)[1], " model\nAICc: ", data$model[1, "aicc"], "; log.lik: ", data$model[1, "log.lik"])
        }
    } else {
        plot_main <- dots$main
    }

    ## Get the plotting parameters
    options(warn = -1)
    plot_params <- get.plot.params(data, data_params, cent.tend, quantiles,
                    rarefaction_level = NULL,
                    type = "continuous",
                    elements_args = list(elements = FALSE),
                    observed_args = list(observed = FALSE),
                    xlab = ifelse(is.null(dots$xlab), "Time", dots$xlab),
                    ylab = ifelse(is.null(dots$ylab), "Disparity", dots$ylab),
                    main = plot_main)
    options(warn = 0)

    ## Plot the simulated data
    plot.continuous(plot_params, data_params, add = add, density = density)
}

plot.test.metric <- function(data, specific.args, ...) {

    ## Adding slopes
    add.slope <- function(model, col) {
        ## Get the slope parameters
        slope_param <- try.get.from.model(model, "Estimate")

        ## Plot the model
        if(!any(is.na(slope_param)) || !is.null(slope_param)) {
            ## Add the slope
            abline(a = slope_param[1],
                   b = slope_param[2],
                   col = col)
        }
    }
    ## Adding fits
    add.fit <- function(model) {
        fit_param <- try.get.from.model(model, "r.squared")
        if(!is.null(fit_param) || length(fit_param) != 0) {

            if(any(names(fit_param) == "adj.r.squared")) {
                fit_param <- fit_param$adj.r.squared
                is_adjusted <- TRUE
            } else {
                is_adjusted <- FALSE
            }

            return(paste0(ifelse(is_adjusted, "Adj. R^2: ", "R^2: "), unlist(round(fit_param, 3))))
        } else {
            return(NA)
        }
    }

    ## Detect whether to plot the shift steps or not
    shift_plots <- !is.null(data$saved_steps)
    if(shift_plots) {
        ## Find which steps to plot
        if(!is.null(specific.args$visualise.steps)) {
            check.class(specific.args$visualise.steps, c("numeric", "integer"))
            steps_to_visualise <- specific.args$visualise.steps
            if(any(check <- steps_to_visualise > n.subsets(data$saved_steps[[1]]))) {
                stop(paste0("Impossible to display the step", ifelse(sum(check) > 1, "s ", " "), paste0(steps_to_visualise[check], collapse = ", "), " because the test only contains ",  n.subsets(data$saved_steps[[1]]), " steps."), call. = FALSE)
            }
        } else {
            steps_to_visualise <- floor(seq(from = 1, to = n.subsets(data$saved_steps[[1]]) - 1, length.out = 4))
        }
        n_steps <- length(steps_to_visualise)
    }

    ## Setting plot window size
    group_plot <- sapply(names(data$results), function(x) strsplit(x, "\\.")[[1]][[1]])

    ## Separating the plots in different groups (per plot windows)
    n_plots <- length(unique(group_plot))

    if(!shift_plots) {
        if(n_plots > 1){
            ## Correct the number of plots if only 3
            plot_size <- ifelse(n_plots == 3, 4, n_plots)

            ## Setting up plotting window
            op_tmp <- par(mfrow = c(ceiling(sqrt(plot_size)),floor(sqrt(plot_size))))
        }
    } else {    
        ## Create the template of step plots
        steps_to_consider <- ifelse(n_steps == 3, 4, n_steps)
        steps_plots <- results_plots <- base_matrix <- matrix(0, ceiling(sqrt(steps_to_consider)), floor(sqrt(steps_to_consider)), byrow = TRUE)
        steps_plots[1:n_steps] <- 1:n_steps
        steps_plots[1:n_steps] <- steps_plots[1:n_steps] + n_plots
        steps_plots <- t(steps_plots)
        ## Create the template for the normal plots
        results_plots <- base_matrix
        results_plots[] <- 1
        ## Create the columns of for layout template
        if(n_plots > 1) {
            for(i in 2:n_plots) {
                ## Add a row to the step plots
                tmp <- base_matrix
                tmp[1:n_steps] <- steps_plots[1:n_steps] + max(steps_plots[1:n_steps], na.rm = TRUE) + 1 - min(steps_plots[1:n_steps], na.rm = TRUE)
                steps_plots <- rbind(steps_plots, tmp)
                ## Add a row to the normal plots
                tmp <- base_matrix
                tmp[] <- i
                results_plots <- rbind(results_plots, tmp)
            }
        }

        ## Create the layout
        set_layout <- layout(cbind(results_plots, steps_plots))
        # layout.show(set_layout)
        ## Parameter backup
        op_tmp <- NULL
    }

    ## Get the plotting arguments
    plot_args <- list(...)

    ## Get the ylim
    if(is.null(plot_args$ylim)) {
        plot_args$ylim <- range(unlist(lapply(data$results, function(x) x[,2])), na.rm = TRUE)
    }
    ## Get the colours
    if(is.null(plot_args$col)) {
        plot_args$col <- c("black", "orange", "blue")
    } else {
        if((missing_cols <- (3 - length(plot_args$col))) > 0) {
            plot_args$col <- c(plot_args$col, rep(plot_args$col, missing_cols))
        }
    }
    ## get the title
    if(is.null(plot_args$xlab)) {
        plot_args$xlab <- "Amount of data considered (%)"
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- as.character(as.expression(data$call$metric))
    }             
    ## get the point size
    if(is.null(plot_args$pch)) {
        plot_args$pch <- 19
    } 

    ## Figuring out the legend
    if(is.null(specific.args$legend)) {
        plot_legend <- TRUE
        legend_args <- list()
    } else {
        if(is(specific.args$legend, "logical")) {
            plot_legend <- specific.args$legend
            legend_args <- list()
        } else {
            plot_legend <- TRUE
            legend_args <- specific.args$legend
        }
    }
    ## Checking out if it's two lists
    if(length(legend_args == 2) && !is.null(names(legend_args[[1]])) && !is.null(names(legend_args[[2]]))) {
        if(names(legend_args[[1]]) == names(legend_args[[2]])) {
            legend_args_1 <- legend_args[[1]]
            legend_args_2 <- legend_args_2_base <- legend_args[[2]]
        }
    } else {
        legend_args_1 <- legend_args_2 <- legend_args_2_base <- legend_args
    }


    ## Separating the data
    plot_groups <- list()
    for(group in 1:length(unique(group_plot))) {
        plot_groups[[group]] <- data$results[grep(unique(group_plot)[[group]], names(data$results))]
    }

    ## Separating the models
    if(!is.null(data$models)) {
        model_groups <- list()
        for(group in 1:length(unique(group_plot))) {
            model_groups[[group]] <- data$models[grep(unique(group_plot)[[group]], names(data$models))]
        }
    }

    ## Plot all the results
    for(one_plot in 1:n_plots) {

        ## Get the data to plot
        plot_data <- plot_groups[[one_plot]]

        ## Setting the specific plot arguments
        one_plot_args <- plot_args

        ## get the xlim
        if(is.null(plot_args$xlim)) {
            one_plot_args$xlim <- range(as.numeric(plot_data[[1]][,1]))
        }
        ## Get the title
        if(is.null(plot_args$main)) {
            one_plot_args$main <- unique(group_plot)[[one_plot]]
        }                   

        ## Data to plot
        one_plot_args$x <- plot_data[[1]][,1]
        one_plot_args$y <- plot_data[[1]][,2]

        ## Which kind of plot
        if(length(plot_data) == 1) {
            ## Set the colours
            col_vector <- plot_args$col[1]
            one_plot_args$col <- col_vector
            do.call(plot, one_plot_args)
        } else {
            ## Set the coulours
            col_vector <- plot_args$col[-1]

            ## First part
            one_plot_args$col <- col_vector[1]
            do.call(plot, one_plot_args)
            ## Second part
            one_plot_args$x <- plot_data[[2]][,1]
            ## Add a small shift
            step <- abs(mode.val(diff(one_plot_args$x)))
            one_plot_args$x <- one_plot_args$x + (step*0.05)
            one_plot_args$y <- plot_data[[2]][,2]
            one_plot_args$col <- col_vector[2]
            do.call(points, one_plot_args)
                        
            ## Set up the legend arguments
            if(plot_legend) {
                if(is.null(legend_args_1$x)) {
                    legend_args_1$x <- "bottomright"
                }
                if(is.null(legend_args_1$legend)) {
                    legend_args_1$legend <- names(plot_data)
                }
                if(is.null(legend_args_1$pch)) {
                    legend_args_1$pch <- plot_args$pch
                }
                if(is.null(legend_args_1$col)) {
                    legend_args_1$col <- col_vector
                }
                ## Plot the legend
                do.call(legend, legend_args_1)
            }
        }

        ## Adding the fit and models
        if(!is.null(data$models)) {

            ## Get the fit of the first model
            fit <- add.fit(model_groups[[one_plot]][[1]])
            ## Slope for the first model
            add.slope(model_groups[[one_plot]][[1]], col = col_vector[1])

            ## Get the eventual second fit
            if(!is.null(model_groups[[one_plot]][[1]])) {
                ## Fit for the second model
                fit <- c(fit, add.fit(model_groups[[one_plot]][[2]]))
                ## Slope for the second model
                add.slope(model_groups[[one_plot]][[2]], col = col_vector[2])
            }

            ## Add the fits as a legend
            if(!all(na_fit <- is.na(fit))) {

                ## Set up the legend arguments
                if(plot_legend) {
                    if(is.null(legend_args_2$x)) {
                        legend_args_2$x <- "topright"
                    }
                    if(is.null(legend_args_2$legend)) {
                        legend_args_2$legend <- fit[!na_fit]
                    }
                    if(is.null(legend_args_2$lty)) {
                        legend_args_2$lty <- c(1,1)[!na_fit]
                    }
                    if(is.null(legend_args_2$col)) {
                        legend_args_2$col <- col_vector[!na_fit]
                    }
                    ## Plot the legend
                    do.call(legend, legend_args_2)
                    ## Reinitialise the legend
                    legend_args_2 <- legend_args_2_base
                }
            }
        }
    }

    ## Add the steps visualisation
    if(shift_plots) {

        ## List of plot margins
        mar_base <- c(2,2,2,1)
        xaxts <- yaxts <- rep("n", n_steps)
        yaxts[c(nrow(base_matrix), length(base_matrix))-1] <- "s"
        xaxts[c(ncol(base_matrix)+1, length(base_matrix))] <- "s"

        ## Select the shifts
        if(length(data$saved_steps)/2 != round(length(data$saved_steps)/2)) {
            ## There is the random one
            shift_list <- c(1, seq(from = 2, to = length(data$saved_steps), by = 2))
        } else {
            ## There is no random one
            shift_list <- c(seq(from = 1, to = length(data$saved_steps), by = 2))
        }

        ## Loop through the shifts
        for(shift in shift_list) {

            ## Loop through the different steps
            for(step in 1:n_steps){
                ## Selecting the subsets
                step_preview <- get.subsets(data$saved_steps[[shift]],steps_to_visualise[step])
                ## Making a new subset without the negatives
                step_preview$subsets$negatives$elements <- matrix((1:nrow(step_preview$matrix[[1]]))[-c(step_preview$subsets[[1]]$elements)], ncol = 1)

                ## Default title
                step_name <- names(step_preview$subsets)[1]
                ## Selecting the colours and the title
                if(names(data$saved_steps)[[shift]] == "random") {
                    select_col <- c(plot_args$col[1], "grey")
                    step_name <- paste0(step_name, "%")
                } else {
                    select_col <- plot_args$col[-1]
                    step_name <- paste0(step_name, "% - ", as.character(100-as.numeric(step_name)), "%")
                }
                                    
                ## Plotting the shift preview
                par(mar = mar_base)
                plot.dispRity(step_preview, type = "preview", col = select_col, specific.args = list(legend = FALSE), main = step_name,xaxt = xaxts[step], yaxt = yaxts[step])
            }
        }
        ## Reset the default plot margins
        par(mar = c(5, 4, 4, 2) + 0.1)
    }

    ## Restoring the parameters
    if(n_plots > 1 && !is.null(op_tmp)) {
        par(op_tmp)
    }
}