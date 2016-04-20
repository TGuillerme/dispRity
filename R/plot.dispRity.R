#' @title dispRity object plotting
#'
#' @description Plots a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param type Either \code{"continuous"} (\code{"c"}), \code{"box"} (\code{"b"}), \code{"lines"} (\code{"l"}) or \code{"polygon"} (\code{"p"}). When unspecified, is set to \code{"continuous"} if \code{\link{time.series}} is used with \code{method = "continuous"}, else is set to \code{"box"}. See details.
#' @param quantile The quantiles to display (default is \code{quantile = c(50, 95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[base]{mean}}).
#' @param rarefaction Either a \code{logical} whether to rarefy the data; or an \code{integer} for setting a specific rarefaction level or \code{"plot"} to plot the rarefaction curves.
#' @param elements \code{logical} whether to plot the number of elements per series.
#' @param time.series \code{logical} whether to handle continuous data from the \code{time.series} function as time (in Ma).
#' @param observed \code{logical} whether to plot the observed values or not (if existing; default is \code{FALSE}).
#' @param add \code{logical} whether to add the new plot an existing one (default is \code{FALSE}).
#' @param density the density of shading lines to be passed to \code{link[graphics]{polygon}}. Is ignored if \code{type = "box"} or \code{type = "lines"}.
#' @param ... Any optional arguments to be passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' \code{type}:
#' \itemize{
#'   \item \code{"continuous"}: plots the results as a continuous line.
#'   \item \code{"box"}: plots the results as discrete box plots (note that this option ignores the user set quantiles and central tendency).
#'   \item \code{"lines"}: plots the results as discrete vertical lines with the user's set quantiles and central tendency.
#'   \item \code{"polygon"}: identical as \code{"lines"} but using polygons rather than vertical lines.
#' }
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_tree) ; data(BeckLee_mat50)
#' data(BeckLee_mat99) ; data(BeckLee_ages)
#'
#' ## Setting the data
#' ## Generate 5 equidistant time slices in the data set assuming gradual
#' ## evolutionary models
#' sliced_data <- time.series(data = BeckLee_mat99, tree = BeckLee_tree,
#'      method = "continuous", model = "acctran", time = 5,
#'      FADLAD = BeckLee_ages)
#' bootstrapped_data <- boot.matrix(sliced_data, bootstraps = 20,
#'      rarefaction = TRUE)
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#' 
#' ## Discrete plotting
#' plot(sum_of_variances, type = "box")
#' 
#' ## Using polygons rather than boxes (quantiles and central tendency can be
#' ## set by the user)
#' plot(sum_of_variances, type = "polygon", quantile = c(10, 50, 95),
#'      cent.tend = mode.val)
#' 
#' ## Using different options
#' plot(sum_of_variances, type = "lines", elements = TRUE, ylim = c(0, 5),
#'      xlab = ("Time (Ma)"), ylab = "disparity", ylim = c(0, 5))
#' 
#' ## Continuous plotting (all default options)
#' plot(sum_of_variances, type = "continuous")
#' 
#' ## Using different options (with non time.slicing option)
#' plot(sum_of_variances, type = "continuous", time.series = FALSE,
#'      elements = TRUE, col = c("red", "orange", "yellow"))
#' 
#' ## Rarefactions plots
#' plot(sum_of_variances, rarefaction = "plot")
#' 
#' @seealso \code{\link{dispRity}} and \code{\link{summary.dispRity}}.
#'
#' @author Thomas Guillerme

#Testing
#source("sanitizing.R")
#source("plot.dispRity_fun.R")

plot.dispRity<-function(data, type, quantile=c(50,95), cent.tend=mean, rarefaction=FALSE, elements=FALSE, ylim, xlab, ylab, col, time.series=TRUE, observed=FALSE, add=FALSE, density=NULL, ...){

    #SANITIZING
    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #must have 5 elements
    check.length(data, 5, " must be a 'dispRity' object.")
    #must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a 'dispRity' object.")
    OBSresults <- data$disparity$observed
    #is the data bootstrapped?   
    if(!is.na(match("bootstraps", names(data$data)))) {
        #must have more than one bootstrap!
        if(length(data$data$bootstrap[[1]][[1]]) > 1) {
            is.bootstrapped <- TRUE
            BSresults <- data$disparity$bootstrapped
        } else {
            is.bootstrapped <- FALSE
        }
    } else {
        is.bootstrapped <- FALSE
    }
    #check if is.distribution
    is.distribution <- ifelse(length(data$disparity$observed[[1]][[1]][[1]]) == 1, FALSE, TRUE)

    #quantile
    #Only check if the data is bootstrapped
    if(is.bootstrapped == TRUE) {
        check.class(quantile, "numeric", " must be any value between 1 and 100.")
        #remove warnings
        options(warn = -1)
        if(any(quantile) < 1) {
            stop("Quantile(s) must be any value between 1 and 100.")
        }
        if(any(quantile) > 100) {
            stop("Quantile(s) must be any value between 1 and 100.")
        }
        options(warn = 0)
    }

    #cent.tend
    #Must be a function
    check.class(cent.tend, "function")
    #The function must work
    silent <- check.metric(cent.tend)

    #type
    if(missing(type)) {
        #Set type to default
        if(any(grep("continuous method", data$call))) {
            type <- "continuous"
        } else {
            type <- "box"
        }
    } else {
        #type must be either "discrete", "d", "continuous", or "c"
        all_types <- c("continuous", "c", "box", "b", "lines", "l", "polygon", "p")
        #type must be a character string
        check.class(type, "character")
        #type must have only one element
        check.length(type, 1, paste(" argument must be either a user's function or one of the following:\n", paste(all_types, collapse=", "), ".", sep=""))
        if(all(is.na(match(type, all_types)))) stop(paste("Type argument must be either a user's function or one of the following:\n", paste(all_types, collapse=", "), ".", sep=""))
        
        #if type is a letter change it to the full word (lazy people...)
        if(type == "c") type <- "continuous"
        if(type == "b") type <- "box"
        if(type == "l") type <- "lines"
        if(type == "p") type <- "polygon"
    }

    #If continuous, set time to continuous Ma (default)
    if(type == "continuous" & time.series == TRUE) {
        #Check if time.slicing was used (saved in call)
        if(any(grep("Data was split using continuous method", data$call))) {
            time_slicing <- data$series
            xlab <- "Time (Mya)"
        } else {
            time_slicing <- FALSE
        }
    } else {
        time_slicing <- FALSE
    }

    #elements
    #must be logical
    if(class(elements) != "logical") {
        if(elements != "log") {
            stop("Diversity must be either a logical or 'log'.")
        } else {
            elements <- TRUE
            div.log <- TRUE
        }
    } else {
        div.log <- FALSE
    }

    #rarefaction
    #Set to null (default)
    which.rare <- NULL
    #if rarefaction is "plot", plot the rarefaction curves
    if(rarefaction != "plot") {
    #Else, make sure rarefaction works    
        #must be logical
        if(class(rarefaction) == "logical") {
            logic.rare <- TRUE
            if(rarefaction == TRUE) {
                which.rare <- "min"
            } else {
                which.rare <- "max"
            }
        } else {
            check.class(rarefaction, "numeric", " must be either logical or a single numeric value.")
            check.length(rarefaction, 1, " must be either logical or a single numeric value.")
            which.rare <- rarefaction
        }
    } else {
        #Rarefaction plot
        which.rare <- "plot"
        #Cancel plot type
        type <- "rarefaction"
    }

    #Test if rarefaction data exists!
    if(which.rare != "max") {
        if(any(grep("rarefied", data$call)) == FALSE) {
            stop("Data set is not rarefied. Use rarefaction = FALSE.")
        }
    }

    #xlab
    if(missing(xlab)) { 
        xlab <- "default"
    } else {
        #length must be 1
        check.length(xlab, 1, " must be a character string.")
    }

    #ylab
    if(missing(ylab)) {
        ylab <- "default"
    } else {
        #length must be 
        if(elements == FALSE) {
            check.length(ylab, 1, " must be a character string.")
        } else {
            if(length(ylab) > 2) stop("ylab can have maximum two elements.")
        }
    }

    #col
    #if default, is ok
    if(missing(col)) {
        if(type == "box") {
            col <- "white"
        } else {
            col <- "default"
        }
    } else {
        check.class(col, "character", " must be a character string.")
    }

    #ylim
    if(missing(ylim)) {
        ylim <- "default"
    } else {
        check.class(ylim, "numeric")
        check.length(ylim, 2, " must be a vector of two elements.")
    }

    #add
    check.class(add, "logical")

    #density
    if(type == "continuous" || type == "polygon") {
        if(!is.null(density)) {
            check.class(density, "numeric")
            check.length(density, 1, " must be a single numeric value.")
        }
    }

    #PREPARING THE PLOT

    #summarising the data
    summarised_data <- summary.dispRity(data, quantile = quantile, cent.tend = cent.tend, rounding = 5)

    #Check the rarefaction
    if(which.rare != "max") {
        if(length(unique(summarised_data$n)) == 1) {
            if(which.rare == "plot") {
                stop("Data is not rarefied!")
            }
            rarefaction <- FALSE
            which.rare <- "max"
        }
    }

    #Rarefaction must be in summarised_data
    if(class(which.rare) == "numeric") {
        #The data must have the right number of rarefaction elements
        elements_in <- length(which(unlist(summarised_data$n) == which.rare))
        elements_req <- length(unique(unlist(summarised_data[[1]])))
        if(elements_in != elements_req) {
            stop(paste("Rarefaction: only ", elements_in, "/" ,elements_req, " series have at least ", which.rare, " elements.", sep=""))
        }
    }

    #Check continuous (set to discrete if only one series)
    if(which.rare != "plot") {
        if(type == "continuous") {
            if(length(unique(summarised_data$series)) == 1) {
                type <- "box"
                message('Only one series of data available: type is set to "box".')
            }
        }
    }

    #Setting the default arguments
    default_arg <- set.default(summarised_data, data$call, elements = elements, ylim = ylim, xlab = xlab, ylab = ylab, col = col, which.rare = which.rare)
    ylim <- default_arg[[1]]
    xlab <- default_arg[[2]]
    ylab <- default_arg[[3]]
    col <- default_arg[[4]]

    #PLOTTING THE RESULTS

    if(which.rare == "plot") {
        #How many rarefaction plots?
        n_plots <- length(unique(summarised_data[,1]))

        #Open the multiple plots
        op_tmp <- par(mfrow = c(ceiling(sqrt(n_plots)),round(sqrt(n_plots))))

        #Rarefaction plots
        for(nPlot in 1:n_plots) {
            tmp_summarised_data <- get.series(summarised_data, rare_level = nPlot)
            plot.rarefaction(tmp_summarised_data, which.rare, ylim, xlab, ylab, col, main = level_name, ...)
            #plot.rarefaction(tmp_summarised_data, which.rare, ylim, xlab, ylab, col, main = level_name) ; warning("DEBUG: plot")
        }

        #Done!
        par(op_tmp)

    } else { 

        #Continuous plot
        if(type == "continuous") {
            if(elements == FALSE) {
                saved_par <- plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, time_slicing, observed, add, density,...)
                #saved_par <- plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, time_slicing, observed, add, density) ; warning("DEBUG: plot")
            } else {
                #bigger_margin<-par(mar=c(5,4,4,4))
                saved_par <- plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, time_slicing, observed, add, density, ...)
                #saved_par <- plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, time_slicing, observed, add, density) ; warning("DEBUG: plot")
                par(new = TRUE)
                plot.elements(summarised_data, which.rare, ylab = ylab, col = col, type, div.log, cex.lab = saved_par$cex.lab, ...)
                #plot.elements(summarised_data, which.rare, ylab = ylab, col = col, type, div.log, cex.lab = saved_par$cex.lab) ; warning("DEBUG: plot")
                #par(bigger_margin)
            }

        } else {

            #Box plot
            if(type == "box") {
                #Simple case: boxplot
                plot_data <- transpose.box(data, which.rare)
                boxplot(plot_data, ylim = ylim, xlab = xlab, ylab = ylab, col = col, add, ...)
                #boxplot(plot_data, ylim = ylim, xlab = xlab, ylab = ylab, col = col, add) ; warning("DEBUG: plot")

                if(observed == TRUE) {
                    if(any(!is.na(extract.summary(summarised_data, 3, which.rare)))){
                        #Add the points observed (if existing)
                        for(point in 1:length(plot_data)) {
                            x_coord <- point
                            y_coord <- extract.summary(summarised_data, 3, which.rare)[point]
                            points(x_coord, y_coord, pch = 4, col = col[length(col)])
                        }
                    }
                }
                    

            } else {

                #Personalised discrete plots
                if(elements == FALSE) {
                    saved_par <- plot.discrete(summarised_data, which.rare, type, ylim, xlab, ylab, col, observed, add, density, ...) 
                    #saved_par <- plot.discrete(summarised_data, which.rare, type, ylim, xlab, ylab, col, observed, add, density) ; warning("DEBUG: plot")
                } else {
                    #bigger_margin<-par(mar=c(5,4,4,4))
                    saved_par <- plot.discrete(summarised_data, which.rare, type, ylim, xlab, ylab, col, observed, add, density, ...)
                    #saved_par <- plot.discrete(summarised_data, which.rare, type, ylim, xlab, ylab, col, observed, add, density, cex.lab = 0.1) ; warning("DEBUG: plot")
                    par(new = TRUE)
                    plot.elements(summarised_data, which.rare, ylab = ylab, col = col, type, div.log, cex.lab = saved_par$cex.lab, ...)
                    #plot.elements(summarised_data, which.rare, ylab = ylab, col = col, type, div.log, cex.lab = saved_par$cex.lab) ; warning("DEBUG: plot")
                    #par(bigger_margin)
                }
            }   
        }

    }
    #End
}
