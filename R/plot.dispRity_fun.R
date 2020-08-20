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
    if((!is.na(data$call$subsets["trees"])) && (as.numeric(data$call$subsets["trees"]) > 1)) {
        summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5, na.rm = TRUE)
    } else {
        summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5)
    }

    ## Find the observed data rows (that are not rarefactions)
    observed_data <- !is.na(summarised_data$obs)
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
            rarefaction_match <- which(summarised_data$n_1 == rarefaction_level
                                        &&
                                       summarised_data$n_2 == rarefaction_level)
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
    name_part <- c(1, ifelse(data_params$between.groups, 3, 2))
    disparity$names <- selected_data[, name_part]
    disparity$data  <- selected_data[, -name_part]

    ## Update the data to be in boxplot format
    if(type == "box") {
        ## Extracting boxplot data
        if(is.null(rarefaction_level)) {
            ## Getting the observed or bootstrapped data
            if(data_params$bootstrap && !observed_args$observed) {
                ## Getting the bootstrapped data
                box_data <- do.call(cbind, unlist(extract.dispRity(data, observed = FALSE), recursive = FALSE))
            } else {
                if(data_params$distribution) {
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
        disparity$data <- box_data
    }

    ## Set up the helpers options
    helpers <- list()
    ## Detect the number of quantiles
    helpers$n_quantiles <- (ncol(disparity$data) - ifelse(data_params$bootstrap, 2, 1))/2
    helpers$n_points <- length(data$disparity)

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
            options$ylim[1] <- options$ylim[1] - options$ylim[1]*percent_change
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
                elements_args$col <- "black"
            } else {
                elements_args$col <- options$col[[1]]
            }
        }
        if(is.null(elements_args$pch)) {
            elements_args$pch <- 15
        }
        if(is.null(elements_args$lty)) {
            elements_args$lty <- 2
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
        points_args$y <- points_args$data$obs
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

    ## Add the values
    add_args$x <- 1:plot_params$helpers$n_points
    add_args$y <- plot_params$disparity$names[,2]

    ## Scale the y values
    percent_change <- 0.02
    rescaling <- plot_params$options$ylim
    rescaling[1] <- rescaling[1] + percent_change * rescaling[1]
    rescaling[2] <- rescaling[2] - percent_change * rescaling[2]
    add_args$y <- scales::rescale(add_args$y, to = rescaling)

    ## If between groups, add the values for the other groups
    if(data_params$between.groups) {
        add_args$y <- plot_params$disparity$names[,3]
    }

    ## Check if ylab exists
    if(is.null(plot_params$elements_args$ylab)) {
        axis_label <- ifelse(length(plot_params$options$ylab) > 1, plot_params$options$ylab[2], "Elements")
    } else {
        axis_label <- plot_params$elements_args$ylab
    }

    ## Add the different types
    if(type == "continuous") {
        add_args$lty <- plot_params$elements_args$lty
        do.call(lines, add_args)
    } else {
        add_args$pch <- plot_params$elements_args$pch
        do.call(points, add_args)
    } 

    ## Add the axis label
    seq.along.range <- function(range, out = 5) {
        seq(from = range(range)[1],
            to = range(range)[2],
            length.out = out)
    }
    
    ## Add the second y axis
    axis(4, at = seq.along.range(rescaling), labels = seq.along.range(plot_params$disparity$names[,2]),
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
                    plot_args$lwd <- cis * 1.5
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
        axis(1, 1:plot_params$helpers$n_points, plot_params$disparity$names$subsets)
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

    ## Set up the specific args
    if(is.null(specific.args$matrix)) {
        specific.args$matrix <- 1
    }
    if(is.null(specific.args$dimensions)) {
        specific.args$dimensions <- c(1,2)
    }

    ## Capturing the dots options
    plot_args <- list(...)

    ## Setting the dimensions
    plot_args$x <- data$matrix[[specific.args$matrix]][, specific.args$dimensions[1]]
    plot_args$y <- data$matrix[[specific.args$matrix]][, specific.args$dimensions[2]]

    ## Getting the loadings
    loading <- apply(data$matrix[[specific.args$matrix]], 2, var, na.rm = TRUE)
    loading <- round(loading/sum(loading)*100, 2)

    ## Setting the labels
    if(is.null(plot_args$xlab)) {
        plot_args$xlab <- paste0("Dimension ", specific.args$dimensions[1], " (", loading[specific.args$dimensions[1]], "%)")
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- paste0("Dimension ", specific.args$dimensions[2], " (", loading[specific.args$dimensions[2]], "%)")
    }

    ## Setting plot limits
    plot_lim <- range(as.vector(c(data$matrix[[specific.args$matrix]][, specific.args$dimensions])))
    if(is.null(plot_args$xlim)) {
        plot_args$xlim <- plot_lim
    }
    if(is.null(plot_args$ylim)) {
        plot_args$ylim <- plot_lim
    }

    ## Setting the pch
    if(is.null(plot_args$pch)) {
        plot_args$pch <- 19
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

    ## Make a colour classifier
    col_order <- plot_args$col
    if(n_groups > 1) {
        classifier <- rep(NA, nrow(data$matrix[[specific.args$matrix]]))
        for(class in 1:n_groups) {
            classifier[data$subsets[[class]]$elements[,1]] <- class
        }
        plot_args$col <- plot_args$col[classifier]
    }

    ## Plot the results
    do.call(plot, plot_args)
    if(n_groups > 1) {
        legend("topright", legend = names(data$subsets), col = col_order, pch = plot_args$pch, cex = 0.666)
    }

    ## Return invisible
    return(invisible())
}

## The following is a modified version of plot.randtest from ade4 v1.4-3
plot.randtest <- function(data_sub, ...) {
    plot_args <- list(...)

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

    ## Adding the legend (test results)
    legend("topleft", bty = "n", legend = c("p-value", round(data_sub$pvalue, 5)), cex = 0.7, adj = 0.2)
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
        if(make.metric(cent.tend, silent = TRUE) != "level1") {
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