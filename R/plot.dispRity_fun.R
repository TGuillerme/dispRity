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
get.plot.params <- function(data, data_params, cent.tend, quantiles, xlab, ylab, ylim, col, rarefaction_level, elements, type, observed_args, ...) {

    ## Set up the plotting data
    ## Summarise the data
    if((!is.na(data$call$subsets["trees"])) && (as.numeric(data$call$subsets["trees"]) > 1)) {
        summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5, na.rm = TRUE)
    } else {
        summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5)
    }

    ## Get the observed data (no rarefaction)
    observed_data <- !is.na(summarised_data$obs)
    if(any(!observed_data)) {
        ## Find true rarefactions
        check.na.all.row <- function(row, na_to_test, data) {
            if(!na_to_test[row]) {
                na_to_test[row] <- all(is.na(data[row, ]))
            }
            return(na_to_test[row])
        }
        ## Finding the TRUE observed data
        observed_data <- sapply(1:nrow(summarised_data), check.na.all.row, observed_data, summarised_data)
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
        ## Get both the observed and rarefied data
        selected_data <- summarised_data[rarefaction_match ,]
    } else {
        ## Just get the observed data
        selected_data <- summarised_data[observed_data,]
    }

    ## Separate the data elements
    disparity <- list()
    name_part <- c(1, ifelse(data_params$between.groups, 3, 2))
    disparity$names <- selected_data[, name_part]
    disparity$data  <- selected_data[, -name_part]

    ## Set up the helpers options
    helpers <- list()
    ## Detect the number of quantiles
    helpers$n_quantiles <- (ncol(disparity$data) - ifelse(data_params$bootstrap, 2, 1))/2
    helpers$n_points <- length(data$disparity)

    ## Set up the plotting options
    dots <- list(...)
    options <- list()

    ## Set the xlabel
    if(is.null(xlab)) {
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
        check.class(xlab, "character", " must be a character string.")
        check.length(xlab, 1, " must be a character string.")
        options$xlab <- xlab
    }

    ## Set the ylabel
    if(is.null(ylab)) {
        ## Default is the metric name
        options$ylab <- as.character(data$call$disparity$metrics$name)
        if(elements) {
            ## Default for element is "elements"
            options$ylab <- "Elements"
        }
    } else {
        ## User input        
        check.class(ylab, "character", " must be a character string.")
        if(length(ylab) > 2) stop.call("", "ylab can have maximum of two elements.")
        options$ylab <- ylab
    }

    ## Set the y limits
    if(is.null(ylim)) {
        if(type != "rarefaction") {
            ## Get the range of the data
            options$ylim <- range(disparity$data, na.rm = TRUE)
            ## Add 2% on each side
            precent_change <- 0.02
            options$ylim[1] <- options$ylim[1] - options$ylim[1]*precent_change
            options$ylim[2] <- options$ylim[2] + options$ylim[2]*precent_change
        } else {
            options$ylim <- "rarefaction"
        }
    } else {
        ## User input
        check.class(ylim, c("numeric", "integer"))
        check.length(ylim, 2, " must be a vector of two elements.")
        options$ylim <- ylim
    }

    ## Set the colours
    if(is.null(col)) {
        ## For boxplots, the default colour is white
        if(type == "box" && type != "rarefaction") {
            options$col <- "white"
        } else {
            options$col <- "black"
        }
    } else {
        ## User input
        check.class(col, "character", " must be a character string.")
        options$col <- col
    }
    ## Add potential missing colours
    if(helpers$n_quantiles > 0) {
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
        observed$names <- selected_data[, name_part]
        observed$data  <- selected_data[, -name_part]

        ## Default observed arguments
        if(is.null(observed_args$col)) {
            observed_args$col <- options$col[[1]]
        }
        if(is.null(observed_args$pch)) {
            observed_args$pch <- 4
        }
        if(is.null(observed_args$cex)) {
            observed_args$cex <- 1
        }
    }

    ## Output the finalised list
    return(list("disparity" = disparity, "helpers" = helpers, "options" = options, "observed_args" = observed_args))
}


## discrete plotting
plot.discrete <- function(summarised_data, rarefaction, is_bootstrapped, is_distribution, type, ylim, xlab, ylab, col, observed, obs_list_arg, add, density, ...) {


    ## How many points?
    plot_params$helpers$n_points <- length(unique(summarised_disparity$subsets))

    ## dummy matrix (for getting the nice boxplots split + column names)
    dummy_mat <- matrix(1:plot_params$helpers$n_points, ncol = plot_params$helpers$n_points)
    colnames(dummy_mat) <- extract.from.summary(summarised_data, 1)

    ## Empty plot
    if(add == FALSE) {
        boxplot(dummy_mat, col = "white", border = "white", ylim = ylim, ylab = ylab[[1]], xlab = xlab, boxwex = 0.001,, type = "n", ...)
        #boxplot(dummy_mat, col = "white", border = "white", ylim = ylim, ylab = ylab[[1]], xlab = xlab, boxwex = 0.001, type = "n") ; warning("DEBUG: plot")
    }

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

    ## Check if bootstrapped
    if(is_bootstrapped || is_distribution) {
        ## How many quantiles?
        n_quantiles <- (ncol(summarised_data) - ifelse(is_bootstrapped, 4, 3))/2

        ## Set the width (default)
        width <- 0.5 #plot_params$helpers$n_points/(plot_params$helpers$n_points*2)

        ## Set the colours
        if(length(col) < (n_quantiles + 1)) {
            cols_missing <- (n_quantiles + 1) - length(col)
            colfun <- colorRampPalette(c("grey", "lightgrey"))
            col_tmp <- c(col, colfun(cols_missing))
            poly_col <- col_tmp[-1]
            poly_col <- rev(poly_col)
        } else {
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        }


        ## Add the quantiles
        if(type == "polygon") {
            for (point in 1:plot_params$helpers$n_points) {
                for(cis in 1:n_quantiles) {
                    ## Setting X
                    x_vals <- c(point-width/(n_quantiles - cis + 1.5), point+width/(n_quantiles - cis + 1.5), point+width/(n_quantiles - cis + 1.5), point-width/(n_quantiles - cis + 1.5)) + shift
                    ## Setting Y
                    y_vals <- c(rep(extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3) + cis, rarefaction)[point], 2),
                              rep(extract.from.summary(summarised_data, (ifelse(is_bootstrapped, 4, 3) + n_quantiles*2) - (cis - 1), rarefaction)[point], 2)
                              )
                    ## Plotting the box
                    polygon(x_vals, y_vals, col = poly_col[[cis]], border = col[[1]], density)

                }
            }
        }
        if(type == "line") {
            for (point in 1:plot_params$helpers$n_points) {
                for(cis in 1:n_quantiles) {
                    ## Setting Y
                    y_vals<-c(extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3) + cis, rarefaction)[point],
                              extract.from.summary(summarised_data, (ifelse(is_bootstrapped, 4, 3) + n_quantiles*2) - (cis - 1), rarefaction)[point])
                    ## Plotting the box
                    lines(x = rep((point + shift), 2), y = y_vals, lty = (n_quantiles - cis + 1), lwd = cis * 1.5, col = col[[1]])

                }
            }
        }
        ## Add the points estimates
        points(1:plot_params$helpers$n_points + shift, extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), col = col[[1]], pch = 19)
    } else {
        
        ## Add the points estimates
        points(1:plot_params$helpers$n_points + shift, extract.from.summary(summarised_data, 3, rarefaction), col = col[[1]], pch = 19)
    }


    if(observed == TRUE) {
        ## Add the points observed (if existing)
        points(1:plot_params$helpers$n_points + shift, extract.from.summary(summarised_data, 3, rarefaction = FALSE), col = obs_list_arg$col, pch = obs_list_arg$pch, cex = obs_list_arg$cex)
    }

    ## Save parameters
    return(par())
}

## continuous plotting
plot.continuous <- function(plot_params, add, density, ...) {
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

    ## Plot the central tendency
    if(!add) {

        ## Setting up the current plot parameters
        current_plot <- plot_params$options
        current_plot$x <- seq(from = 1, to = plot_params$helpers$n_points)-shift
        current_plot$y <- plot_params$disparity$data[, 2] ; warning("select with is_bootstrapped or is_distribution")
        current_plot$type <- "l"
        current_plot$col <- plot_params$options$col[[1]]
        current_plot$xaxt <- "n"

        ## Plot the thingy
        do.call(plot, current_plot)

        ## Add the axis labels
        axis(1, 1:plot_params$helpers$n_points, plot_params$disparity$names$subsets)

    } else {
        ## Change from here
        lines(seq(from = 1, to = plot_params$helpers$n_points), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), col = col[[1]])
    }

    ## Check if bootstrapped
    if(is_bootstrapped || is_distribution) {
        ## How many quantiles?
        n_quantiles <- (ncol(summarised_data) - ifelse(is_bootstrapped, 4, 3))/2

        ## Set the colours
        if(length(col) < (n_quantiles + 1)) {
            cols_missing <- (n_quantiles + 1) - length(col)
            colfun <- colorRampPalette(c("grey", "lightgrey"))
            col_tmp <- c(col, colfun(cols_missing))
            poly_col <- col_tmp[-1]
            poly_col <- rev(poly_col)
        } else {
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        }

        ## Add the polygons
        for (cis in 1:n_quantiles) {
            x_vals <- c(1:plot_params$helpers$n_points, plot_params$helpers$n_points:1)
            y_vals <- c(extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3) + cis, rarefaction), rev(extract.from.summary(summarised_data, (ifelse(is_bootstrapped, 4, 3) + n_quantiles*2)-(cis-1), rarefaction)))

            ## Dividing the polygon if NAs
            if(any(is.na(y_vals))) {

                ## Check where the NAs are
                is_nas <- is.na(y_vals[1:plot_params$helpers$n_points])

                ## Selecting the groups of applicable data
                groups <- numeric()
                group_label <- 1
                for(point in 1:length(is_nas)) {
                    if(is.na(y_vals[point])) {
                        group_label <- group_label + 1
                        groups[point] <- NA
                    } else {
                        groups[point] <- group_label
                    }
                }

                ## splitting the data into the groups
                y_vals1 <- split(y_vals[1:plot_params$helpers$n_points], groups)
                y_vals2 <- split(y_vals[(plot_params$helpers$n_points+1):(plot_params$helpers$n_points*2)], rev(groups))
                x_vals1 <- split(x_vals[1:plot_params$helpers$n_points], groups)
                x_vals2 <- split(x_vals[(plot_params$helpers$n_points+1):(plot_params$helpers$n_points*2)], rev(groups))

                ## Merging the groups
                y_vals <- mapply(c, y_vals1, y_vals2, SIMPLIFY = FALSE)
                x_vals <- mapply(c, x_vals1, x_vals2, SIMPLIFY = FALSE)

                ## Plotting the polygons
                mapply(polygon, x_vals, y_vals, MoreArgs = list(col = poly_col[[cis]], border = "NA", density = density))

            } else {
                polygon(x_vals, y_vals, col = poly_col[[cis]], border = "NA", density)
            }

        }

        ## Add the central tendency on top
        lines(seq(from = 1, to = plot_params$helpers$n_points), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), lty = 1, col = col[[1]])

        ## Add the observed values on top
        if(observed == TRUE) {
            ## Add the points observed (if existing)
            points(1:plot_params$helpers$n_points, extract.from.summary(summarised_data, 3, rarefaction = FALSE), col = obs_list_arg$col, pch = obs_list_arg$pch, cex = obs_list_arg$cex)
        }
    }

    ##  Save parameters
    return(par())
}

## Plotting elements
plot.elements <- function(summarised_data, rarefaction, type, ylab, col, element.pch, ...) {

    ## Check if ylab2 exists
    if(length(ylab) == 1) {
        ylab[[2]] <- "Elements"
    }

    ## Add the lines
    if(type == "continuous") {
        ## Continuous (straightforward)
        plot(extract.from.summary(summarised_data, 2, rarefaction), type = "l", lty = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    } else {
        ## Creating the dummy data table
        plot_params$helpers$n_points <- length(unique(summarised_disparity$subsets))
        dummy_mat <- matrix(extract.from.summary(summarised_data, 2, rarefaction), ncol = plot_params$helpers$n_points)
        colnames(dummy_mat) <- extract.from.summary(summarised_data, 1)
        boxplot(dummy_mat, xaxt = "n", yaxt = "n", xlab = "", ylab = "", boxwex = 0.5/plot_params$helpers$n_points, lty = 2, border = "white", type = "n")
        for(line in 1:plot_params$helpers$n_points) {
            # lines(c(line-0.25, (line+0.25)), rep(summarised_data[line,2], 2), lty = 2, lwd = 1.5)
            points(line, summarised_data[line,2], pch = element.pch, col = col)
        }
    }
    ## lines(extract.from.summary(summarised_data, 2, rarefaction), lty=2)
    axis(4, lty = 2)
    mtext(ylab[[2]], side = 4, line = 2)
}


## Splitting the summarised data table by subsets (list)
split.summary.data <- function(subsets_levels, summarised_data) {
    return(summarised_data[which(summarised_disparity$subsets == subsets_levels),])
}

## rarefaction plottings
plot.rarefaction <- function(sub_data, ylim, xlab, ylab, col, main, ...) {
    ## Get parameters
    if(ylim[[1]] == "rarefaction") {
        ## ylim?
        ylim <- c(min(sub_data[, -c(1:2)], na.rm = TRUE) - min(sub_data[, -c(1:2)], na.rm = TRUE) * 0.02 , max(sub_data[, -c(1:2)], na.rm = TRUE) + max(sub_data[, -c(1:2)], na.rm = TRUE) * 0.02)
    }
    ## title?
    if(missing(main)) {
        main <- unique(as.character(sub_disparity$subsets))
    }
    ## how many quantiles?
    n_quantiles <- (ncol(sub_data) - 4)/2

    ## colors?
    if(length(col) < n_quantiles) {
        col <- rep(col[[1]], n_quantiles + 1)
    }

    ## Plot central tendency curve (continuous)
    # if(!missing(main)) {
        plot(rev(sub_data[, 4]), type = "l",  xlab = xlab, ylab = ylab[[1]], col = col[[1]], ylim = ylim, main = main, ...)
    # } else {
        # plot(rev(sub_data[,4]), type = "l",  xlab = xlab, ylab = ylab[[1]], col = col[[1]], ylim = ylim, ...)
    # }


    ## Plot the quantiles curves
    if(n_quantiles != 0) {
        for (cis in 1:n_quantiles) {
            ## lower quantile
            lines(rev(sub_data[, 4 + cis]), lty = (n_quantiles + 2 - cis), col = col[[cis + 1]])
            ## upper quantile
            lines(rev(sub_data[, ncol(sub_data) - (cis - 1)]), lty = (n_quantiles + 2- cis), col = col[[cis + 1]])
        }
    }
    ##  Save parameters
    return(par())
}

## Transposing data for boxploting (taking functions from summary.dispRity)
transpose.box <- function(data, rarefaction, is_bootstrapped) {

    get.rare <- function(data, rare){
        return(data[[rare]])
    }

    if(rarefaction == FALSE) {
        if(is_bootstrapped) {
            ## Select the raw data
            box_data <- lapply(disparity$disparity, function(X) return(X[[2]]))
        } else {
            box_data <- lapply(disparity$disparity, function(X) return(X[[1]]))
        }
    } else {
        ## Select the rarefaction data
        rare_rows <- lapply(lapply(disparity$subsets, lapply, nrow), function(X) which(X[-1] == rarefaction) + 1)
        box_data <- mapply(get.rare, disparity$disparity, rare_rows, SIMPLIFY = FALSE)
    }

    ## Get the subset lengths
    subset_length <- unique(unlist(lapply(disparity$subsets, function(x) return(length(x[[1]])))))
    if(length(subset_length) == 1){
        ## All data has the same length
        output <- t(matrix(unlist(box_data), nrow = length(disparity$subsets), byrow = TRUE))
        colnames(output) <- names(disparity$subsets)
    } else {
        ## Data has different lengths
        output <- box_data
    }

    return(output)
}

## The following is a modified version of plot.randtest from ade4 v1.4-3
plot.randtest <- function (data_sub, nclass = 10, coeff = 1, ...) {
    
    ## Observed data
    observed <- data_sub$obs
    ## Hist info
    histogram <- data_sub$plot$hist
    ## Plot info
    xlim <- data_sub$plot$xlim
    ylim <- c(0, max(data_sub$plot$hist$count))

    ## Plotting the simulated data
    plot(data_sub$plot$hist, xlim = xlim, col = grey(0.8), ...)

    ## Adding the observed data
    lines(c(observed, observed), c(ylim[2]/2, 0))
    points(observed, ylim[2]/2, pch = 18, cex = 2)

    ## Adding the legend (test results)
    legend("topleft", bty = "n", legend = c("p-value", round(data_sub$pvalue, 5)), cex = 0.7, adj = 0.2)
}

## Plotting model tests results
plot.model.test.support <- function(data, col, ylab, ylim, ...) {

    ## Extracting the weighted aicc
    plot_aic <- disparity$aic.models[, 3]

    ## Ordering the weighted aicc
    ordered_aic <- plot_aic[order(plot_aic, decreasing = TRUE)]

    ## Plot
    plotcoords <- graphics::barplot(ordered_aic, col = col, ylim = ylim, ylab = ylab, ...)
}

## Plotting a space preview
plot.preview <- function(data, dimensions, matrix, xlab, ylab, ylim, col, ...) {

    ## The "ggplot" colours
    gg.color.hue <- function(n) {
        grDevices::hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
    }

    ## Capturing the dots options
    plot_args <- list(...)

    ## Setting the dimensions
    plot_args$x <- disparity$matrix[[matrix]][, dimensions[1]]
    plot_args$y <- disparity$matrix[[matrix]][, dimensions[2]]

    ## Getting the loadings
    loading <- apply(disparity$matrix[[matrix]], 2, var, na.rm = TRUE)
    loading <- round(loading/sum(loading)*100, 2)

    ## Setting the labels
    if(missing(xlab)) {
        plot_args$xlab <- paste0("Dimension ", dimensions[1], " (", loading[dimensions[1]], "%)")
    } else {
        plot_args$xlab <- xlab
    }
    if(missing(ylab)) {
        plot_args$ylab <- paste0("Dimension ", dimensions[2], " (", loading[dimensions[2]], "%)")
    } else {
        plot_args$ylab <- ylab
    }

    ## Setting plot limits
    plot_lim <- range(as.vector(c(disparity$matrix[[matrix]][, dimensions])))
    if(is.null(plot_args$xlim)) {
        plot_args$xlim <- plot_lim
    }
    if(missing(ylim)) {
        plot_args$ylim <- plot_lim
    } else {
        plot_args$ylim <- ylim
    }

    ## Setting the pch
    if(is.null(plot_args$pch)) {
        plot_args$pch <- 19
    }

    ## Get the number of colour groups
    n_groups <- length(disparity$subsets)

    ## Setting the colours
    if(missing(col)) {
        if(n_groups == 1) {
            plot_args$col <- "black"
        } else {
            if(disparity$call$subsets[[1]] == "customised") {
                plot_args$col <- gg.color.hue(n_groups)
            } else {
                plot_args$col <- grDevices::heat.colors(n_groups+2)[1:n_groups]
            }
        }
    } else {
        plot_args$col <- col
    }

    ## Make a colour classifier
    if(n_groups > 1) {
        classifier <- rep(NA, nrow(disparity$matrix[[matrix]]))
        for(class in 1:n_groups) {
            classifier[disparity$subsets[[class]]$elements[,1]] <- class
        }
        col_order <- plot_args$col
        plot_args$col <- plot_args$col[classifier]
    }

    ## Plot the results
    do.call(plot, plot_args)
    if(n_groups > 1) {
        legend("topright", legend = names(disparity$subsets), col = col_order, pch = plot_args$pch, cex = 0.666)
    }

    ## Return invisible
    return(invisible())
}



# ~~~~~~~~~~
# sequential.test plots
# ~~~~~~~~~~


## Adding a line
# add.line <- function(xs, ys, lines.args) {
#     if(!is.null(lines.args)) {
#         ## Adding the x,y coordinates
#         lines.args$x <- xs ; lines.args$y <- ys
#         do.call(lines, lines.args)
#     } else {
#         lines(xs, ys)
#     }
# }

## Adding significance tokens
# significance.token <- function(xs, ys, p.value, token.args) {
#     if(p.value < 0.1) {
#         ## Selecting the token
#         if(p.value < 0.1) token <- "."
#         if(p.value < 0.05) token <- "*"
#         if(p.value < 0.01) token <- "**"
#         if(p.value < 0.001) token <- "***"
#         ## Default plotting
#         if(is.null(token.args)) {
#             text(x = sum(xs)/2, y = max(ys)+max(ys)*0.05, token)
#         } else {
#         ## Plotting with arguments
#             token.args$labels <- token
#             token.args$x <- sum(xs)/2
#             if(any(names(token.args) == "float")) {
#                 token.args$y <- max(ys)+max(ys)*token.args$float
#                 token.args$float <- NULL
#             } else {
#                 token.args$y <- max(ys)+max(ys)*0.05
#             }
#             do.call(text, token.args)
#         }
#     }
# }

## Getting the two coordinates of the intercepts (intercept0 and intercept predicted)
# get.intercept.coords <- function(results_out, model_number, is.distribution, significance) {
#     if(is.distribution != TRUE) {
#         ## Get the first y coordinate (first intercept)
#         y1 <- results_out$Intercept[model_number, 1]
#         ## Test if intercept0 is significant
#         if(model_number == 1) {
#             if(results_out$Intercept[model_number, 4] > 0.05) {
#                 y1 <- 0
#             }
#         }

#         ## Get the second y coordinate (second intercept)
#         if(model_number < nrow(results_out$Intercept)) {
#             ## Intercept already estimated
#             y2 <- results_out$Intercept[model_number+1, 1]
#         } else {
# #             ## Estimate the intercept
# #             if(results_out$Slopes[model_number, 4] < 0.05) {
# #                 slope <- results_out$Slopes[model_number, 1]
# #             } else {
# #                 slope <- 0
# #             }
# #             y2 <- intercept.estimate(results_out$Intercepts[model_number,1], slope)
# #         }
# #     } else {
# #         ## Get the first y coordinate
# #         if(model_number == 1) {
# #             ## Get the initial (estimated) intercept if significant
# #             if(results_out$Intercept$Initial[4, significance] < 0.05) {
# #                 y1 <- unlist(results_out$Intercept$Initial[1, significance])
# #             } else {
# #                 y1 <- 0
# #             }
# #         } else {
# #             ## Get the predicted intercept
# #             y1 <- unlist(results_out$Intercepts$Predicted[model_number-1, significance])
# #         }

# #         ## Get the second y coordinate (second intercept)
# #         if(model_number-1 < nrow(results_out$Intercept$Predicted)) {
# #             ## Intercept already estimated
# #             y2 <- unlist(results_out$Intercepts$Predicted[model_number, significance])
# #         } else {
# #             ## Estimate the intercept
# #             if(results_out$Slopes$`Pr(>|t|)`[model_number, significance] < 0.05) {
# #                 slope <- unlist(results_out$Slopes$Estimate[model_number, significance])
# #             } else {
# #                 slope <- 0
# #             }
# #             y2 <- intercept.estimate(unlist(results_out$Intercepts$Predicted[model_number-1, significance]), slope)
# #         }        
# #     }

# #     ## Return the coordinates
# #     return(c(y1, y2))
# # }

# ## Plotting the results of sequential tests
# # plot.seq.test <- function(results_out, is.distribution, significance, lines.args, token.args) {
# #     ## Get the number of models to plot
# #     if(is.distribution != TRUE) {
# #         n_models <- nrow(results_out$Slopes)
# #     } else {
# #         n_models <- nrow(results_out$Slopes$Estimate)
# #     }

# #     ## Loop through each model
# #     for(model_number in 1:n_models) {
# #         ## Getting x,y coordinates for one model
# #         x_coords <- c(model_number, model_number+1)
# #         y_coords <- get.intercept.coords(results_out, model_number=model_number, is.distribution, significance)

# #         ## Plotting the line
# #         add.line(x_coords, y_coords, lines.args)

# #         ## Add significance (if necessary)
# #         if(is.distribution != TRUE) {
# #             p_value <- results_out$Slope[model_number, 4]
# #         } else {
# #             p_value <- results_out$Slope$`Pr(>|t|)`[model_number, significance]
# #             ## get p_value
# #         }

# #         significance.token(x_coords, y_coords, p_value, token.args)
# #     }
# # }


# #Plot sequential.test shortcut
# # if(length(class(data)) == 2) {
# #     if(is(data, "dispRity") && is(data, "seq.test")) {

# #         #lines.args sanitizing
# #         if(!is.null(lines.args)) check.class(lines.args, "list")

# #         #token.args sanitizing
# #         if(!is.null(token.args)) check.class(token.args, "list")

# #         #Creating the table results
# #         results_out <- summary.seq.test(data, quantiles, cent.tend, recall, digits = 10, results = "coefficients", match_call = list(cent.tend = NULL))

# #         #Checking if distribution
# #         is_distribution <- ifelse(length(disparity$models[[1]]) == 1, FALSE, TRUE)

# #         #significance sanitizing
# #         if(is_distribution == TRUE) {
# #             if(is(significance, "character")) {
# #                 if(significance != "cent.tend") {stop("significance argument must be either 'cent.tend' or a single 'numeric' value.")}
# #                 significance = 1
# #             } else {
# #                 check.class(significance, "numeric", " must be either 'cent.tend' or a single 'numeric' value.")
# #                 check.length(significance, 1, " must be either 'cent.tend' or a single 'numeric' value.")
# #                 if(is.na(match(significance, seq(from = 1, to = length(quantiles)*2)))) {
# #                     stop("significance argument must be the number of the quantile (e.g. 1 for the first quantile).")
# #                 } else {
# #                     significance = significance + 1
# #                 }
# #             }
# #         }


# #         #Plotting the results
# #         if(add != TRUE) {
# #             #subsamples
# #             subsamples <- unique(unlist(strsplit(names(disparity$models), split = " - ")))
# #             #Get the all the intercepts estimate
# #             if(is_distribution == TRUE) {
# #                 all_intercepts <- unlist(c(results_out$Intercepts$Initial[1,significance], results_out$Intercepts$Predicted[,significance], intercept.estimate(unlist(results_out$Intercepts$Predicted[(length(subsamples)-2),significance]), unlist(results_out$Slopes$Estimate[(length(subsamples)-1),significance]))))
# #             } else {
# #                 all_intercepts <- c(results_out$Intercepts[,1], intercept.estimate(results_out$Intercepts[(length(subsamples)-1),1], results_out$Slopes[(length(subsamples)-1),1]))
# #             }
            
# #             if(missing(xlab)) {
# #                 xlab <- "subsamples"
# #             }
# #             if(missing(ylab)) {
# #                 ylab <- "Estimated disparity"
# #             }

# #             #Empty plot
# #             subsamples_length <- length(subsamples)
# #             plot(seq(from = 1, to = subsamples_length), all_intercepts, col = "white", xlab = xlab, ylab = ylab, xaxt = "n", ...)
# #             #plot(seq(from = 1, to = subsamples_length), all_intercepts, col = "white", xlab = xlab, ylab = ylab, xaxt = "n") ; warning("DEBUG in plot.dispRity")
# #             axis(1, at = 1:subsamples_length, labels = subsamples)
# #         }

# #         plot.seq.test(results_out, is_distribution, significance, lines.args, token.args)

# #     }

