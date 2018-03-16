#' @title dispRity object plotting
#'
#' @description Plots a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param ... Any optional arguments to be passed to \code{\link[graphics]{plot}}.
#' @param type Either \code{"continuous"} (\code{"c"}), \code{"box"} (\code{"b"}), \code{"line"} (\code{"l"}) or \code{"polygon"} (\code{"p"}). When unspecified, is set to \code{"continuous"} if \code{\link{chrono.subsets}} is used with \code{method = "continuous"}, else is set to \code{"box"}. See details.
#' @param quantiles The quantiles to display (default is \code{quantiles = c(50, 95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[stats]{median}}).
#' @param rarefaction Either \code{NULL} (default) or \code{FALSE} for not using the rarefaction scores; a \code{numeric} value of the level of rarefaction to plot; or \code{TRUE} for plotting the rarefaction curves.
#' @param elements \code{logical} whether to plot the number of elements per subsets.
#' @param ylim Optional, two \code{numeric} values for the range of the y axis.
#' @param xlab Optional, a \code{character} string for the caption of the x axis.
#' @param ylab Optional, one or two (if \code{elements = TRUE}) \code{character} string(s) for the caption of the y axis.
#' @param col Optional, some \code{character} string(s) for the colour of the graph.
#' @param chrono.subsets \code{logical} whether to handle continuous data from the \code{chrono.subsets} function as time (in Ma). When this option is set to TRUE for other \code{type} options, the names of the subsets are used for the x axis labels.
#' @param observed \code{logical} whether to add the observed values on the plot as crosses (default is \code{FALSE}).
#' @param add \code{logical} whether to add the new plot an existing one (default is \code{FALSE}).
#' @param density the density of shading lines to be passed to \code{\link[graphics]{polygon}}. Is ignored if \code{type = "box"} or \code{type = "line"}.
#' @param element.pch optional, if \code{elements = TRUE}, the point type to represent them (default are squares: \code{element.pch = 15})
# ' @param significance when plotting a \code{\link{sequential.test}} from a distribution, which data to use for considering slope significance. Can be either \code{"cent.tend"} for using the central tendency or a \code{numeric} value corresponding to which quantile to use (e.g. \code{significance = 4} will use the 4th quantile for the level of significance ; default = \code{"cent.tend"}).
# ' @param lines.args when plotting a \code{\link{sequential.test}}, a list of arguments to pass to \code{\link[graphics]{lines}} (default = \code{NULL}).
# ' @param token.args when plotting a \code{\link{sequential.test}}, a list of arguments to pass to \code{\link[graphics]{text}} for plotting tokens (see details; default = \code{NULL}).
#' @param nclass when plotting a \code{\link{null.test}} the number of \code{nclass} argument to be passed to \code{\link[graphics]{hist}} (default = \code{10}).
#' @param coeff when plotting a \code{\link{null.test}} the coefficient for the magnitude of the graph (see \code{\link[ade4]{randtest}}; default = \code{1}).
#'
#' @details
#' The different \code{type} arguments are:
#' \itemize{
#'   \item \code{"continuous"}: plots the results as a continuous line.
#'   \item \code{"box"}: plots the results as discrete box plots (note that this option ignores the user set quantiles and central tendency).
#'   \item \code{"line"}: plots the results as discrete vertical lines with the user's set quantiles and central tendency.
#'   \item \code{"polygon"}: identical as \code{"line"} but using polygons rather than vertical lines.
#' }
#' 
#TG: The following is from sequential.test (not implemented yet)
# The \code{token.args} argument intakes a list of arguments to be passed to \code{\link[graphics]{text}} for plotting the significance tokens. The plotted tokens are the standard p-value significance tokens from R:
# \code{0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1}
# Additionally, the \code{float} argument can be used for setting the height of the tokens compared to the slopes. For example one can use \code{token.args = list(float = 0.3, col = "blue", cex = 0.5))} for plotting blue tokens 50% smaller than normal and 30% higher than the slope.
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
#' ## Using different options (with non time.slicing option)
#' plot(disparity, type = "continuous", chrono.subsets = FALSE,
#'      elements = TRUE, col = c("red", "orange", "yellow"))
#' 
#' ## Rarefactions plots
#' plot(disparity, rarefaction = TRUE)
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

#Testing
# source("sanitizing.R")
# source("plot.dispRity_fun.R")
# data(BeckLee_mat50)
# groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12), rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_subsets <- custom.subsets(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 3)
# sum_of_variances <- dispRity(bootstrapped_data, metric =  variances)
# subsets <- extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = FALSE)
# data <- sequential.test(subsets, family = gaussian, correction = "hommel")
# data <- sum_of_variances
# quantiles=c(50, 95)
# cent.tend=median
# rarefaction = NULL
# elements = FALSE
# chrono.subsets = FALSE
# observed = FALSE
# add = FALSE
# density = NULL
# nclass = 10
# coeff = 1
# significance="cent.tend"
# lines.args=NULL
# token.args=NULL

# data(disparity)
# data <- dispRity
# type = "line"
# elements = TRUE
# ylim = c(0, 5)
# xlab = ("Time (Ma)")
# ylab = "disparity"

plot.dispRity <- function(x, ..., type, quantiles = c(50, 95), cent.tend = median, rarefaction = NULL, elements = FALSE, ylim, xlab, ylab, col, chrono.subsets = TRUE, observed = FALSE, add = FALSE, density = NULL, element.pch = 15, nclass = 10, coeff = 1){ #significance="cent.tend", lines.args=NULL, token.args=NULL

    data <- x

    #SANITIZING
    #DATA
    if(length(class(data)) > 1) {

        ## randtests plots
        if(class(data)[[1]] == "dispRity" && class(data)[[2]] == "randtest") {
            ## sanitising
            check.class(nclass, "numeric")
            check.class(coeff, "numeric")
            check.length(nclass, 1, " must be a single numeric value.")
            check.length(coeff, 1, " must be a single numeric value.")
            
            ## length_data variable initialisation
            length_data <- length(data)
            
            ## Set up the plotting window
            ## Open the multiple plots
            if(length_data != 1) {
                op_tmp <- par(mfrow = c(ceiling(sqrt(length_data)), round(sqrt(length_data))))

                ## Rarefaction plots
                for(model in 1:length_data) {
                    plot.randtest(data[[model]], nclass = nclass, coeff = coeff, main = paste("MC test for subsets ", names(data)[[model]], sep = ""), ...)
                    ## plot.randtest(data[[model]], nclass = nclass, coeff = coeff, main = paste("MC test for subsets ", names(data)[[model]], sep = "")) ; warning("DEBUG: plot")
                }
                par(op_tmp)
            } else {
                plot.randtest(data[[1]], nclass = nclass, coeff = coeff, ...)
                ## plot.randtest(data[[model]], nclass = nclass, coeff = coeff) ; warning("DEBUG: plot")
            }
        }

        ## dtt plots (from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R)
        if(class(data)[[1]] == "dispRity" && class(data)[[2]] == "dtt") {

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
                    stop("quantiles(s) must be any value between 0 and 100.")
                }
                quantiles_n <- length(quantiles)

                ## Check the central tendency
                check.class(cent.tend, "function")
                ## The function must work
                if(make.metric(cent.tend, silent = TRUE) != "level1") {
                    stop("cent.tend argument must be a function that outputs a single numeric value.")
                }


                ## Summarised data
                quantiles_values <- apply(data$sim, 1, quantile, probs = CI.converter(quantiles))
                cent_tend_values <- apply(data$sim, 1, cent.tend)

                ## Plotting the polygons for each quantile
                for (cis in 1:quantiles_n) {
                    xx <- c(data$times, rev(data$times))
                    yy <- c(quantiles_values[(quantiles_n*2) - (cis-1), ], rev(quantiles_values[cis ,]))
                    polygon(xx, yy, col = col[cis+1],  border = FALSE, density = density)
                }


                ## Add the central tendency
                lines(data$times, cent_tend_values, col = col[1], lty = 2)
            }

            ## Add the observed disparity
            lines(data$times, data$dtt, col = col[1], lwd = 1.5)
        } 

         # if(class(data)[[1]] == "dispRity" && class(data)[[2]] == "model.test") {
         #    ## Colours
         #    if(missing(col)) {
         #        col <- "grey"
         #    }
         #    ## Ylab
         #    if(missing(ylab)) {
         #        ylab <- "Akaike weights"
         #    }
         #    ## Ylim
         #    if(missing(ylim)) {
         #        ylim <- NULL
         #    }

         #    ## Plotting the model support
         #    plot.model.test.support(data = data, col= col, ylab = ylab, ylim = ylim,...)
         # }
        return(invisible())
    }

    ## ----
    ## Normal disparity plot
    ## ----

    ## must be class dispRity
    check.class(data, "dispRity")
    ## must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a 'dispRity' object.")
    ## Check if disparity is a value or a distribution
    is_distribution <- ifelse(length(data$disparity[[1]]$elements) != 1, TRUE, FALSE)
    ## Check the bootstraps
    is_bootstrapped <- ifelse(!is.null(data$call$bootstrap), TRUE, FALSE)

    ## quantiles
    ## Only check if the data is_bootstrapped
    if(is_bootstrapped) {
        check.class(quantiles, "numeric", " must be any value between 1 and 100.")

        ## Are quantiles probabilities or proportions ?
        if(any(quantiles < 1)) {
            ## Transform into proportion
            quantiles <- quantiles*100
        }
        ## Are quantiles proper proportions
        if(any(quantiles < 0) | any(quantiles > 100)) {
            stop("quantiles(s) must be any value between 0 and 100.")
        }
    }

    ## cent.tend
    ## Must be a function
    check.class(cent.tend, "function")
    ## The function must work
    if(make.metric(cent.tend, silent = TRUE) != "level1") {
        stop("cent.tend argument must be a function that outputs a single numeric value.")
    }

    ## type
    # if(length(data$subsets) == 1) {
    #     type <- "box"
    #     message("Only one subset of data available: type is set to \"box\".")
    # }

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

    ## If continuous, set time to continuous Ma (default)
    if(type == "continuous" & chrono.subsets) {
        ## Check if time.slicing was used (saved in call)
        if(data$call$subsets[1] == "continuous") {
            time_slicing <- names(data$subsets)
        }
    } 
    if(!chrono.subsets) {
        time_slicing <- FALSE
    } else {
        time_slicing <- names(data$subsets)
    }

    ## elements
    ## must be logical
    check.class(elements, "logical")

    ## Rarefaction
    if(is.null(rarefaction)) {
        rarefaction <- FALSE
    }
    ## If data is not bootstrapped, rarefaction is FALSE
    if(!is_bootstrapped) {
        rarefaction <- FALSE
    }
    ## Check class
    silent <- check.class(rarefaction, c("logical", "integer", "numeric"))
    if(class(rarefaction) != "logical") {
        ## Right class
        rarefaction <- as.numeric(rarefaction)
        check.length(rarefaction, 1, errorif = FALSE, msg = "Rarefaction must a single numeric value.")
        ## Check if all subsets have the appropriate rarefaction level
        rarefaction_subsets <- lapply(lapply(data$subsets, lapply, nrow), function(X) which(X[-1] == rarefaction)+1)
        ## Check if subsets have no rarefaction
        if(length(unlist(rarefaction_subsets)) != length(data$subsets)) {
            wrong_rarefaction <- lapply(rarefaction_subsets, function(X) ifelse(length(X) == 0, TRUE, FALSE))
            stop(paste("The following subsets do not contain ", rarefaction, " elements: ", paste(names(data$subsets)[unlist(wrong_rarefaction)], collapse = ", "), ".", sep = "" ))
        }
    }

    ## observed
    check.class(observed, "logical")

    ## xlab
    if(missing(xlab)) { 
        xlab <- "default"
        if(!is.null(data$call$subsets) && data$call$subsets != "customised" && chrono.subsets != FALSE && rarefaction != TRUE) {
            xlab <- "Time (Mya)"
        }
    } else {
        ## length must be 1
        check.length(xlab, 1, " must be a character string.")
    }

    ## ylab
    if(missing(ylab)) {
        ylab <- "default"
    } else {
        ## length must be 
        if(elements == FALSE) {
            check.length(ylab, 1, " must be a character string.")
        } else {
            if(length(ylab) > 2) stop("ylab can have maximum of two elements.")
        }
    }

    ## col
    ## if default, is ok
    if(missing(col)) {
        if(type == "box" & rarefaction != TRUE) {
            col <- "white"
        } else {
            col <- "default"
        }
    } else {
        check.class(col, "character", " must be a character string.")
    }

    ## ylim
    if(missing(ylim)) {
        ylim <- "default"
    } else {
        check.class(ylim, "numeric")
        check.length(ylim, 2, " must be a vector of two elements.")
    }

    ## add
    check.class(add, "logical")

    ## density
    if(type == "continuous" || type == "polygon") {
        if(!is.null(density)) {
            check.class(density, "numeric")
            check.length(density, 1, " must be a single numeric value.")
        }
    }

    ## PREPARING THE PLOT

    ## summarising the data
    summarised_data <- summary.dispRity(data, quantiles = quantiles, cent.tend = cent.tend, digits = 5)

    ## Setting the default arguments
    default_arg <- set.default(summarised_data, data, elements = elements, ylim = ylim, xlab = xlab, ylab = ylab, col = col, rarefaction = rarefaction, type = type, is_bootstrapped = is_bootstrapped)
    ylim <- default_arg[[1]]
    xlab <- default_arg[[2]]
    ylab <- default_arg[[3]]
    col <- default_arg[[4]]

    ## PLOTTING THE RESULTS

    ## Rarefaction plot
    if(rarefaction == TRUE) {
        ## How many rarefaction plots?
        n_plots <- length(data$subsets)

        ## Open the multiple plots
        op_tmp <- par(mfrow = c(ceiling(sqrt(n_plots)),round(sqrt(n_plots))))

        ## Rarefaction plots

        ## Get the list of subsets
        subsets_levels <- unique(summarised_data$subsets)

        ## Split the summary table
        sub_summarised_data <- lapply(as.list(subsets_levels), split.summary.data, summarised_data)

        ## Plot the rarefaction curves
        for(nPlot in 1:n_plots) {
            plot.rarefaction(sub_summarised_data[[nPlot]], ylim, xlab, ylab, col, ...)
            # plot.rarefaction(sub_summarised_data[[nPlot]], ylim, xlab, ylab, col) ; warning("DEBUG: plot")
        }

        ## Done!
        par(op_tmp)

        return(invisible())
    }

    ## Continuous plot
    if(type == "continuous") {
        ## Bigger plot margins if elements needed
        if(elements) {
            par(mar = c(5, 4, 4, 4) + 0.1)
        }
        saved_par <- plot.continuous(summarised_data, rarefaction, is_bootstrapped, is_distribution, ylim, xlab, ylab, col, time_slicing, observed, add, density,...)
        # saved_par <- plot.continuous(summarised_data, rarefaction, is_bootstrapped, ylim, xlab, ylab, col, time_slicing, observed, add, density) ; warning("DEBUG: plot")
        if(elements) {
            par(new = TRUE)
            plot.elements(summarised_data, rarefaction, ylab = ylab, col = col[[1]], type = "continuous", div.log = FALSE, cex.lab = saved_par$cex.lab, element.pch = element.pch)
        }
        return(invisible())
    }

    ## Polygons or lines
    if(type == "polygon" | type == "line") {
        ## Bigger plot margins if elements needed
        if(elements) {
            par(mar = c(5, 4, 4, 4) + 0.1)
        }
        ## Personalised discrete plots
        saved_par <- plot.discrete(summarised_data, rarefaction, is_bootstrapped, is_distribution, type, ylim, xlab, ylab, col, observed, add, density, ...) 
        # saved_par <- plot.discrete(summarised_data, rarefaction, is_bootstrapped, type, ylim, xlab, ylab, col, observed, add, density) ; warning("DEBUG: plot")
        if(elements) {
            par(new = TRUE)
            plot.elements(summarised_data, rarefaction, ylab = ylab, col = col[[1]], type = "discrete", div.log = FALSE, cex.lab = saved_par$cex.lab, element.pch = element.pch)
        }
        return(invisible())
    }

    ## Box plot
    if(type == "box") {
        ## Simple case: boxplot
        plot_data <- transpose.box(data, rarefaction, is_bootstrapped)
        ## Bigger plot margins if elements needed
        if(elements) {
            par(mar = c(5, 4, 4, 4) + 0.1)
        }
        saved_par <- boxplot(plot_data, ylim = ylim, xlab = xlab, ylab = ylab[[1]], col = col, add = add, ...)
        # saved_par <- boxplot(plot_data, ylim = ylim, xlab = xlab, ylab = ylab[[1]], col = col, add = add) ; warning("DEBUG: plot")

        if(observed == TRUE) {
            if(any(!is.na(extract.from.summary(summarised_data, 3, rarefaction)))){
                ## Add the points observed (if existing)
                for(point in 1:length(plot_data)) {
                    x_coord <- point
                    y_coord <- extract.from.summary(summarised_data, 3, rarefaction)[point]
                    points(x_coord, y_coord, pch = 4, col = "black")
                }
            }
        }
        if(elements) {
            par(new = TRUE)
            plot.elements(summarised_data, rarefaction, ylab = ylab, col = col[[1]], type = "discrete", div.log = FALSE, cex.lab = saved_par$cex.lab, element.pch = element.pch)
        }

        return(invisible())
    }

}