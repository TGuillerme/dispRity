#' @title dispRity object plotting
#'
#' @description Plots a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param ... Any optional arguments to be passed to \code{\link[graphics]{plot}}.
#' @param type Either \code{"continuous"}, \code{"box"}, \code{"line"}, \code{"polygon"} or \code{"space"}. When unspecified, if no disparity was calculated, \code{"preview"} is used. If disparity was calculated, \code{"continuous"} is used for \code{\link{chrono.subsets}} and \code{"box"} for \code{\link{custom.subsets}}. See details.
#' @param quantiles The quantiles to display (default is \code{quantiles = c(50, 95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[stats]{median}}).
#' @param rarefaction Either \code{NULL} (default) or \code{FALSE} for not using the rarefaction scores; a \code{numeric} value of the level of rarefaction to plot; or \code{TRUE} for plotting the rarefaction curves.
#' @param elements \code{logical} whether to plot the number of elements per subsets (default is \code{FALSE}) or a \code{list} of any of the graphical arguments \code{"col"}, \code{"pch"} and/or \code{"cex"}.
#' @param observed \code{logical} whether to add the observed values on the plot as crosses (default is \code{FALSE}) or a \code{list} of any of the graphical arguments \code{"col"}, \code{"pch"} and/or \code{"cex"}.
#' @param add \code{logical} whether to add the new plot an existing one (default is \code{FALSE}).
#' @param density the density of shading lines to be passed to \code{\link[graphics]{polygon}}. Is ignored if \code{type = "box"} or \code{type = "line"}.
#' @param specific.args optional, a named list of arguments to be passed for plotting \code{"dispRity"} objects with more than two classes (usually: * [ ] \code{"randtest"}, \code{"dtt"}, \code{"model.test"}, \code{"model.sim"}, or \code{"test.metric"}) or if the requested plot type is \code{"preview"}. See details.
#'
#' @details
#' The different \code{type} arguments are:
#' \itemize{
#'   \item \code{"continuous"}: plots the results as a continuous line.
#'   \item \code{"box"}: plots the results as discrete box plots (note that this option ignores the user set quantiles and central tendency).
#'   \item \code{"line"}: plots the results as discrete vertical lines with the user's set quantiles and central tendency.
#'   \item \code{"polygon"}: identical as \code{"line"} but using polygons rather than vertical lines.
#'   \item \code{"preview"}: plots two dimensional preview of the space (default is \code{c(1,2)}). WARNING: the plotted dimensions might not be representative of the full multi-dimensional space!
#' }
#' 
#' The different \code{specific.args} arguments for the following options are:
#' \itemize{
#'      \item if \code{type = "preview"}, the default is \code{specific.args = list(dimensions = c(1,2), matrix = 1)} where \code{dimensions} designates which dimensions to plot and \code{matrix} which specific matrix from \code{data} to plot.
#      \item if \code{data} is of class \code{"dispRity"} and \code{"randtest"}, the default is \code{specific.args = list(nclass = 10, coeff = 1)} where \code{nclass} is the number of bars in the histogram and \code{coeff} is the magnitude of the graph.
#' }
#'
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## Discrete plotting
#' plot(disparity, type = "box")
#' 
#' ## Using polygons rather than boxes (quantiles and central tendency can be
#' ## set by the user)
#' plot(disparity, type = "polygon", quantiles = c(10, 50, 95),
#'      cent.tend = mean)
#' 
#' ## Using different options
#' plot(disparity, type = "line", elements = TRUE, ylim = c(0, 5),
#'      xlab = ("Time (Ma)"), ylab = "disparity")
#' 
#' ## Continuous plotting (all default options)
#' plot(disparity, type = "continuous")
#'  
#' ## Rarefactions plots
#' plot(disparity, rarefaction = TRUE)
#' 
#' ## Observed data
#' plot(disparity, observed = TRUE)
#'
#' ## Observed data with graphical details
#' plot(disparity, observed = list("pch" = 19, col = "blue", cex = 4))
#' 
#' \dontrun{
#' ## Geoscale plots
#' require(geoscale)
#' 
#' ## Converting the data into a list
#' data_obs <- extract.dispRity(disparity, observed = TRUE)
#' data_distribution <- extract.dispRity(disparity, observed = FALSE)
#' ## Removing one list level
#' data_distribution <- unlist(data_distribution, recursive = FALSE)
#' data_obs <- as.vector(data_obs)
#' 
#' ## Getting the ages
#' ages <- as.numeric(names(disparity$subsets))
#' 
#' ## Plotting the results median
#' geoscalePlot(ages, data_obs, boxes = "Age", data.lim = c(1.5, 2), type = "l")
#'
#' ## Plotting the results distribution
#' geoscaleBox(data_distribution, ages, boxes = "Age", data.lim = c(1.5, 2))
#' }
#' 
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{summary.dispRity}}, \code{\link{pair.plot}}.
#'
#' @author Thomas Guillerme

# #Testing
# source("sanitizing.R")
# source("plot.dispRity_fun.R")
# data(BeckLee_mat50)
# data(BeckLee_tree)
# data <- custom.subsets(BeckLee_mat50, crown.stem(BeckLee_tree, inc.nodes = FALSE))
# type = "preview"

# data <- customised_subsets
# quantiles=c(50, 95)
# cent.tend=median
# rarefaction = NULL
# elements = FALSE
# observed = FALSE
# add = FALSE
# density = NULL
# nclass = 10
# coeff = 1


# data(disparity)
# data <- disparity
# type = "line"
# ewments = TRUE
# ylim = c(0, 5)
# xlab = ("Time (Ma)")
# ylab = "disparity"

plot.dispRity <- function(x, ..., type, quantiles = c(50, 95), cent.tend = median, rarefaction = NULL, elements = FALSE, observed = FALSE, add = FALSE, density = NULL, specific.args){ #significance="cent.tend", lines.args=NULL, token.args=NULL

    data <- x
    match_call <- match.call()
    dots <- list(...)

    #SANITIZING

    ## Checking specific.args
    if(missing(specific.args)) {
        specific.args <- list()
    } else {
        check.class(specific.args, "list")
    }

    #DATA
    if(length(class(data)) > 1 && !is(data, c("matrix", "array"))) {

        ## Subclass plots

        ## randtests plots
        if(is(data, c("dispRity", "randtest"))) {            
            ## length_data variable initialisation
            length_data <- length(data)

            ## Set up the extra arguments
            dots <- list(...)
            plot_args <- dots

            ## Single plot
            if(length_data == 1) {
                ## Select the right dataset
                plot_args$data_sub <- data[[1]]
                ## Run the plot
                do.call(plot.randtest, plot_args)
            } else {
                ## Set up multiple plot windows
                plot_size <- ifelse(length_data == 3, 4, length_data)
                op_tmp <- par(mfrow = c(ceiling(sqrt(plot_size)), round(sqrt(plot_size))))
                ## All plots
                for(model in 1:length_data) {
                    ## Select the right dataset
                    plot_args$data_sub <- data[[model]]
                    ## Add the title (optional)
                    if(is.null(dots$main)) {
                        plot_args$main <- paste("MC test for subsets ", names(data)[[model]], sep = "")
                    }
                    ## Run the plot
                    do.call(plot.randtest, plot_args)
                }
                par(op_tmp)
            }
            return(invisible())
        }

        ## dtt plots (from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R)
        if(is(data, c("dispRity")) && is(data, c("dtt"))) {
            ## Silence warnings
            options(warn = -1)

            ## Get the ylim
            if(missing(ylim)) {
                ylim <- c(range(pretty(data$dtt)))

                if(!is.null(data$sim)) {
                    ylim_sim <- range(data$sim)
                    ylim <- range(c(ylim, ylim_sim))
                }
            }

            if(missing(xlab)) {
                xlab <- "relative time"
            }
            if(missing(ylab)) {
                ylab <- "scaled disparity"
            }

            if(missing(col)) {
                colfun <- grDevices::colorRampPalette(c("lightgrey", "grey"))
                col <- c("black", colfun(length(quantiles)))
            }

            ## Plot the relative disparity curve
            plot(data$times, data$dtt, xlab = xlab, ylab = ylab, ylim = ylim, type = "n", ...)
            #plot(data$times, data$dtt, xlab = xlab, ylab = ylab, ylim = ylim, type = "n") ; warning("DEBUG plot")

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
                for (cis in 1:n_quantiles) {
                    xx <- c(data$times, rev(data$times))
                    yy <- c(quantiles_values[(n_quantiles*2) - (cis-1), ], rev(quantiles_values[cis ,]))
                    polygon(xx, yy, col = col[cis+1],  border = FALSE, density = density)
                }


                ## Add the central tendency
                lines(data$times, cent_tend_values, col = col[1], lty = 2)
            }

            ## Add the observed disparity
            lines(data$times, data$dtt, col = col[1], lwd = 1.5)

            ## Re-enable warnings
            options(warn = 0)
        } 

        ## model.test plots
        if(is(data, c("dispRity")) && is(data, c("model.test"))) {

            ## Colours
            if(missing(col)) {
                col <- "grey"
            }
            ## Ylab
            if(missing(ylab)) {
                ylab <- "weighted AIC"
            }

            ## Ylim
            if(missing(ylim)) {
                ylim <- NULL
            }

            ## Plotting the model support
            plot.model.test.support(data = data, col = col, ylab = ylab, ylim = ylim, ...)
        }
        
        ## model.sim plots
        if(is(data, c("dispRity")) && is(data, c("model.sim"))) {
            
            ## xlab
            if(missing(xlab)) { 
                xlab <- "default"
            } 

            ## ylab
            if(missing(ylab)) {
                ylab <- "default"
            }

            ## col
            if(missing(col)) {
                col <- "default"
            }
    
            ## ylim
            if(missing(ylim)) {
                ylim <- "default"
            }

            ## add
            check.class(add, "logical")

            ## density
            if(!is.null(density)) {
                check.class(density, "numeric")
                check.length(density, 1, " must be a single numeric value.")
            }

            ## Get inherited subsets (if exist)
            if(!is.null(data$subsets)) {
                subset_names <- rev(data$subsets)
            } else {
                subset_names <- rev(data$simulation.data$fix$subsets)
            }

            ## 
            stop("Rewrite model.sim.plot")

            # ## Preparing the data and the arguments
            # summarised_data <- data.frame(summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5))
            # colnames(summarised_data)[3] <- "obs"
            # summarised_data[,1] <- subset_names

            # ## Setting the default arguments
            # default_arg <- set.default(summarised_data, data, elements = FALSE, ylim = ylim, xlab = xlab, ylab = ylab, col = col, rarefaction = FALSE, type = "continuous", data_params$bootstrap = TRUE, data_params$between.groups = FALSE)
            # ylim <- default_arg[[1]]
            # xlab <- default_arg[[2]]
            # ylab <- default_arg[[3]]
            # if(length(ylab) == 0) {
            #     ylab <- "disparity (simulated)"
            # }
            # col <- default_arg[[4]]

            # ## Plotting the model
            # plot_details <- plot.continuous(summarised_data, rarefaction = FALSE, data_params$bootstrap = TRUE, data_params$distribution = TRUE, ylim, xlab, ylab, col, time_slices = summarised_data$subsets, observed = FALSE, obs_list_arg = NULL, add, density, ...)
        }

        if(is(data, c("dispRity")) && is(data, c("test.metric"))) {

            ## Getting the number of plot groups
            group_plot <- sapply(names(data$results), function(x) strsplit(x, "\\.")[[1]][[1]])

            ## Separating the plots in different groups (per plot windows)
            n_plots <- length(unique(group_plot))

            if(n_plots > 1){
                ## Correct the number of plots if only 3
                plot_size <- ifelse(n_plots == 3, 4, n_plots)

                ## Setting up plotting window
                op_tmp <- par(mfrow = c(ceiling(sqrt(plot_size)),floor(sqrt(plot_size))))
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


            ## Get the yaxis scale
            all_ylim <- if(missing(ylim)) {range(unlist(lapply(data$results, function(x) x[,2])), na.rm = TRUE)} else {ylim}
            #all_ylim <- range(unlist(lapply(data$results, function(x) x[,2])), na.rm = TRUE)

            ## Get the colours
            if(missing(col)) {
                if(any(unique(group_plot) != "random")) {
                    col <- c("orange", "blue")
                } else {
                    col <- "black"
                }
            } else {
                if(length(col) < 2) {
                    col <- c(col, "grey")
                } else {
                    col <- col[1:2]
                }
            }

            ## Plot all the results
            for(one_plot in 1:n_plots) {
                ## Get the data to plot
                plot_data <- plot_groups[[one_plot]]

                ## First plot
                plot(plot_data[[1]],
                    ylim = all_ylim,
                    xlim = if(is.null(dots$xlim)) {range(as.numeric(plot_data[[1]][,1]))} else {xlim},
                    xlab = if(missing(xlab))      {"Amount of data considered (%)"} else {xlab},
                    ylab = if(missing(ylab))      {data$call$metric} else {ylab},
                    pch  = if(is.null(dots$pch))  {19} else {dots$pch},
                    main = if(is.null(dots$main)) {unique(group_plot)[[one_plot]]} else {dots$main},
                    col  = col[1]
                    )

                ## Second plot
                if(length(plot_data) > 1) {
                    plot_data[[2]][,1] <- as.numeric(plot_data[[2]][,1])
                    points(plot_data[[2]],
                           pch = if(is.null(dots$pch))  {19} else {dots$pch},
                           col = col[2])
                    legend("bottomright", legend = names(plot_data),
                           pch = if(is.null(dots$pch))  {19} else {dots$pch},
                           col = col)
                }

                ## Plot the model (if exists)
                if(!is.null(data$models)) {

                    ## Adding slopes and fits
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

                    add.slope(model_groups[[one_plot]][[1]], col = col[1])
                    fit <- add.fit(model_groups[[one_plot]][[1]])
                    
                    if(!is.null(model_groups[[one_plot]][[1]])) {
                        add.slope(model_groups[[one_plot]][[2]], col = col[2])
                        fit <- c(fit, add.fit(model_groups[[one_plot]][[2]]))
                    }

                    ## Add the fits
                    if(!all(na_fit <- is.na(fit))) {
                        legend("topright", legend = fit[!na_fit], lty = c(1,1)[!na_fit], col = col[1:2][!na_fit])
                    }
                }
            }
            if(n_plots > 1) {
                par(op_tmp)
            }
        }
        
        ## Exit subclass plots
        return(invisible())
    }

    ## ----
    ## Normal disparity plot
    ## ----

    ## Special case when the data is a matrix (make a dummy disparity data)
    if(is(data, c("matrix", "array"))) {
        ## Make a minimal dispRity object
        data <- make.dispRity(data)
        ## Set the type to "preview only"
        type <- "preview"
    }

    ## must be class dispRity
    check.class(data, "dispRity")

    ## must have one element called dispRity
    if(!("disparity" %in% names(data))) {
        if(missing(type)) {
            ## Just preview the data
            type <- "preview"
        } else {
            if(type != "preview") {
                stop.call(match_call$x, paste0(" must contain disparity data.\nTry running dispRity(", as.expression(match_call$x), ", ...)"))                
            }
        }
    }

    ## Plot the matrix preview
    if(!missing(type) && type == "preview") {
        ## Plotting the matrix preview
        plot.preview(data, specific.args, ...)
        return(invisible())
    }

    ## Get the dispRity data characteristics
    data_params <- get.data.params(data)

    ## quantiles
    ## Only check if the data data_params$bootstrap or if it's a distribution
    if(data_params$bootstrap || data_params$distribution) {
        check.class(quantiles, c("numeric", "integer"), " must be any value between 1 and 100.")

        ## Are quantiles probabilities or proportions ?
        if(any(quantiles < 1)) {
            ## Transform into proportion
            quantiles <- quantiles*100
        }
        ## Are quantiles proper proportions
        if(any(quantiles < 0) | any(quantiles > 100)) {
            stop.call("", "quantiles(s) must be any value between 0 and 100.")
        }
    }

    ## cent.tend
    ## Must be a function
    check.class(cent.tend, "function")
    ## The function must work
    if(make.metric(cent.tend, silent = TRUE) != "level1") {
        stop.call("", "cent.tend argument must be a function that outputs a single numeric value.")
    }

    ## type
    if(missing(type)) {
        ## Set type to default
        if(any(grep("continuous", data$call$subsets))) {
            type <- "continuous"
        } else {
            type <- "box"
        }
    } else {
        ## type must be either "discrete", "d", "continuous", or "c"
        all_types <- c("continuous", "c", "box", "b", "line", "l", "polygon", "p")
        ## type must be a character string
        check.class(type, "character")
        type <- tolower(type)
        ## type must have only one element
        check.length(type, 1, paste(" argument must only one of the following:\n", paste(all_types, collapse=", "), ".", sep=""))
        check.method(type, all_types, "type argument")
        
        ## if type is a letter change it to the full word (lazy people...)
        type <- ifelse(type == "c", "continuous", type)
        type <- ifelse(type == "b", "box", type)
        type <- ifelse(type == "l", "line", type)
        type <- ifelse(type == "p", "polygon", type)
    }

    ## If data is not bootstrapped, rarefaction is FALSE
    if(!data_params$bootstrap) {
        rarefaction <- NULL
    }

    ## Check rarefaction
    if(!is.null(rarefaction)) {
        rarefaction_class <- check.class(rarefaction, c("logical", "integer", "numeric"))
        if(rarefaction_class != "logical") {
            ## Right class
            rarefaction <- as.numeric(rarefaction)
            check.length(rarefaction, 1, errorif = FALSE, msg = "Rarefaction must a single numeric value.")
            ## Check if all subsets have the appropriate rarefaction level
            rarefaction_subsets <- lapply(lapply(data$subsets, lapply, nrow), function(X) which(X[-1] == rarefaction)+1)
            ## Check if subsets have no rarefaction
            if(length(unlist(rarefaction_subsets)) != length(data$subsets)) {
                wrong_rarefaction <- lapply(rarefaction_subsets, function(X) ifelse(length(X) == 0, TRUE, FALSE))
                stop.call("", paste0("The following subsets do not contain ", rarefaction, " elements: ", paste(names(data$subsets)[unlist(wrong_rarefaction)], collapse = ", "), "."))
            }
        } else {
            if(rarefaction) {
                type <- "rarefaction"
                ## Check if they are enough rarefaction levels
                if(length(data_params$rarefaction) == 1 && data_params$rarefaction != "full") {
                    stop(paste0("Impossible to plot rarefaction curves with only one level of rarefaction. Try to use plot(..., rarefaction = ", data_params$rarefaction[[1]], ") to just see the rarefied data for that level instead."), call. = FALSE)
                }
            }
            rarefaction <- NULL
        }
    }

    ## elements = FALSE
    ## must be logical
    check.class(elements, "logical")
    ## observed = FALSE
    elements_args <- list()
    class_elements <- check.class(elements, c("logical", "list"))
    if(class_elements == "list") {
        ## Transforming into logical and handling the list below
        elements_args <- elements
        elements_args$elements <- TRUE
    } else {
        ## Creating and empty list to be handled below
        elements_args$elements <- elements
    }

    ## observed = FALSE
    observed_args <- list()
    class_observed <- check.class(observed, c("logical", "list"))
    if(class_observed == "list") {
        ## Transforming into logical and handling the list below
        observed_args <- observed
        observed_args$observed <- TRUE
    } else {
        ## Creating and empty list to be handled below
        observed_args$observed <- observed
    }

    ## add = FALSE
    check.class(add, "logical")

    ## PREPARING THE PLOT
    plot_params <- get.plot.params(data = data,
                                   data_params = data_params,
                                   cent.tend = cent.tend,
                                   quantiles = quantiles,
                                   rarefaction_level = rarefaction,
                                   type = type,
                                   observed_args = observed_args,
                                   elements_args = elements_args
                                   , ...)

    ## Set up the plotting task 
    plot_task <- type
    ## The task line is the same as polygon
    if(plot_task == "line") plot_task <- "polygon"

    switch(plot_task,
        "rarefaction" = {
            plot.rarefaction(plot_params, data_params, data)
        },
        "continuous" = {
            plot.continuous(plot_params, data_params, add = add, density = density)
        },
        "polygon" = {
            plot.discrete(plot_params, data_params, add = add, density = density, type = type)
        },
        "box" = {
            ## Set the box arguments
            boxplot_args <- plot_params$options
            boxplot_args$x <- plot_params$disparity$data

            ## Run the box plot
            do.call(boxplot, boxplot_args)
        })

    ## Add the observed
    if(plot_params$observed_args$observed) {
        plot.observed(plot_params)
    }

    ## Add elements
    if(plot_params$elements_args$elements) {
        plot.elements(plot_params, data_params, type)
    }

    return(invisible())
}