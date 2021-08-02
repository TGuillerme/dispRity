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
#' @param specific.args optional, a named list of arguments to be passed for some specific plot types. See details.
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
 #' 
#'      \item if \code{type = "preview"}, the specific arguments can be:
#'      \itemize{
#'          \item \code{dimensions}: two specific dimensions to plot (default is \code{specific.args = list(dimensions = c(1,2)});
#'          \item \code{matrix}: which specific matrix to plot the data from (by default, all the matrices are used).
#'          \item \code{tree}: whether to plot the underlying tree(s) or not. Can be either logical, whether to plot no tree (default is \code{specific.args = list(tree = FALSE)}), all trees (\code{specific.args = list(tree = TRUE)}) or a specific set of trees (e.g. \code{specific.args = list(tree = c(1,2))})
#'      }
#' 
#'      \item for plots with legends (if \code{type = "preview"}; if data is \code{"randtest"} or \code{"test.metric"}) you can pass any non ambiguous arguments for \code{legend} such as \code{specific.args = list(legend = list(x = 1, y = 2, bg = "blue"))}. When the plot generates two legends (e.g. when the data is \code{"test.metric"}), these arguments can be a list (e.g. \code{specific.args = list(legend = list(list(x = "bottomright"), list(x = "topright")))}. \emph{HINT}: to remove the legends all together you can use \code{specific.args = list(legend = FALSE)}.
#' 
#'      \item if data is a \code{"test.metric"} result that was obtained with the option \code{save.steps = TRUE} (see \code{\link{test.metric}}), it is possible to specify which steps to by specifying the following specific argument: \code{specific.args = list(visualise.steps = c(1,4,5))} for visualising steps 1, 4 and 5 of the different shifts. By default, if the \code{"test.metric"} was obtained with the option \code{save.steps = TRUE}, four steps are displayed.
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
#' plot(disparity, type = "line", elements = TRUE, ylim = c(0, 3),
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
#' ## For plotting dispRity objects with the dual classes "randtest", "dtt",
#' ## "model.test", "model.sim" and "test.metric" see the examples
#' ## in the specific function manuals from the "See also" section above
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{summary.dispRity}}, \code{\link{null.test}}, \code{\link{dtt.dispRity}}, \code{\link{model.test}}, \code{\link{model.test.sim}}, \code{\link{test.metric}}
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
# elements = TRUE
# observed = FALSE
# add = FALSE
# density = NULL



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
    if(length(class(data)) > 1 && !(is(data, "matrix") && is(data, "array"))) {

        ## Subclass plots

        ## randtests plots
        if(is(data, "dispRity") && is(data, "randtest")) {
            ## length_data variable initialisation
            length_data <- length(data)

            ## Set up the extra arguments
            dots <- list(...)
            plot_args <- dots
            plot_args$specific.args <- specific.args

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
        
            ## Dtt style plots
            plot.dtt(data, quantiles, cent.tend, density, ...)
            return(invisible())
        } 

        ## model.test plots
        if(is(data, c("dispRity")) && is(data, c("model.test"))) {

            ## Plotting the model support
            plot.model.test(data, ...)
            return(invisible())
        }
        
        ## model.sim plots
        if(is(data, c("dispRity")) && is(data, c("model.sim"))) {

            ## add
            check.class(add, "logical")

            ## density
            if(!is.null(density)) {
                check.class(density, "numeric")
                check.length(density, 1, " must be a single numeric value.")
            }

            plot.model.sim(data, add, density, quantiles, cent.tend, ...)
            return(invisible())
        }

        ## test.metric plots
        if(is(data, c("dispRity")) && is(data, c("test.metric"))) {

            ## Plotting the test.metric results
            plot.test.metric(data, specific.args, ...)
                
            ## Exit subclass plots
            return(invisible())
        }

        ## select.axes plots
        if(is(data, c("dispRity")) && is(data, c("axes"))) {

            ## Plot the data
            plot.axes(data, ...)
                
            ## Exit subclass plots
            return(invisible())
        }        
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
    if(make.metric(cent.tend, silent = TRUE)$type != "level1") {
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