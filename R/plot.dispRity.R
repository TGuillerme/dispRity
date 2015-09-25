#' @title dispRity object plotting
#'
#' @description Plots a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param type Either \code{"continuous"} or \code{"discrete"}.
#' @param CI The confidence intervals values (default is \code{CI = c(50,95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[base]{mean}}).
#' @param rarefaction Either a \code{logical} whether to rarefy the data; or an \code{integer} for setting a specific rarefaction level or \code{"plot"} to plot the rarefaction curves.
#' @param diversity \code{logica} whether to plot the diversity levels (i.e. the number of rows in the matrix).
#' @param discrete_type Either \code{"box"} for boxplots or \code{"line"} for distribution lines.
#' @param ... Any optional arguments to be passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' \code{type}:
#' \itemize{
#'   \item \code{"continuous"}: plots the results in a continuous fashion (e.g. disparity function of time)
#'   \item \code{"discrete"}:plots the results in a discrete fashion (e.g. disparity function of factors)
#' }
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_tree) ; data(BeckLee_mat50) ; data(BeckLee_mat99) ; data(BeckLee_ages)
#'
#' ## Discrete plotting
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
#' ## Caculating the sum of ranges
#' sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, range))
#' ## Plotting the results
#' plot(sum_of_ranges, type = "discrete")
#' ## Same plot with various options
#' plot(sum_of_ranges, type = "discrete", diversity = TRUE, ylab = c("Disparity", "Number of taxa"), xlab = "Factors", discrete_type = "line", main = "Some disparity plot", col = "red")

#' ## Setting the data
#' ## Generate 5 equidistant time slices in the data set assuming gradual evolutionary models
#' sliced_data <- time.series(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = c(120,90,60,30,0), FADLAD = BeckLee_ages)
#' bootstrapped_data <- boot.matrix(sliced_data, bootstraps = 20, rarefaction = TRUE)
#' sum_of_ranges <- dispRity(bootstrapped_data, metric = c(sum, range))
#' 
#' ## Discrete plotting
#' plot(sum_of_ranges, type = "continuous")

#' ## Summarising the results
#' summary(sum_of_ranges) # default
#' ## Using different options
#' summary(sum_of_ranges, CI=75, cent.tend=median, rounding=0)
#' ## Recalling the dispRity parameters
#'  
#' 
#' @seealso \code{\link{dispRity}}
#'
#' @author Thomas Guillerme
plot.dispRity<-function(data, type, CI=c(50,95), cent.tend=mean, rarefaction=FALSE, diversity=FALSE, ylim, xlab, ylab, col, discrete_type="box", ...){

    #SANITIZING
    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #must have 5 elements
    check.length(data, 5, " must be a 'dispRity' object.")
    #must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a dispRity object.")
    results<-data$disparity
    #is the data bootstrapped?   
    if(!is.na(match("bootstraps", names(data)))) {
        #must have more than one bootstrap!
        if(length(data$bootstrap[[1]][[1]]) > 1) {
            is.bootstrapped<-TRUE
        } else {
            is.bootstrapped<-FALSE
        }
    } else {
        is.bootstrapped<-FALSE
    }
    
    #CI
    #Only check if the data is bootstrapped
    if(is.bootstrapped == TRUE) {
        check.class(CI, "numeric", " must be any value between 1 and 100.")
        #remove warnings
        options(warn=-1)
        if(any(CI) < 1) {
            stop("CI must be any value between 1 and 100.")
        }
        if(any(CI) > 100) {
            stop("CI must be any value between 1 and 100.")
        }
        options(warn=0)
    }

    #cent.tend
    #Must be a function
    check.class(cent.tend, "function")
    #The function must work
    silent<-check.metric(cent.tend)

    #type
    #type must be a character string
    check.class(type, "character")
    #type must have only one element
    check.length(type, 1, ' must be either "discrete", "d", "continuous", or "c".')
    #type must be either "discrete", "d", "continuous", or "c"
    all_types <- c("discrete", "d", "continuous", "c")
    if(all(is.na(match(type, all_types)))) stop('type must be either "discrete", "d", "continuous", or "c".')
    
    #if type is "d" or "c", change it to "discrete" or "continuous" (lazy people...)
    if(type == "d") type <- "discrete"
    if(type == "c") type <- "continuous"

    #diversity
    #must be logical
    check.class(diversity, "logical")

    #rarefaction
    #Set to null (default)
    which.rare<-NULL
    #if rarefaction is "plot", plot the rarefaction curves
    if(rarefaction != "plot") {
    #Else, make sure rarefaction works    
        #must be logical
        if(class(rarefaction) == "logical") {
            logic.rare<-TRUE
            if(rarefaction == TRUE) {
                which.rare<-"min"
            } else {
                which.rare<-"max"
            }
        } else {
            check.class(rarefaction, "numeric", " must be either logical or a single numeric value.")
            check.length(rarefaction, 1, " must be either logical or a single numeric value.")
            which.rare<-rarefaction
        }
    } else {
        #Rarefaction plot
        which.rare<-"plot"
        #Cancel plot type
        type<-"rarefaction"
    }

    #xlab
    if(missing(xlab)) { 
        xlab<-"default"
    } else {
        #length must be 1
        check.length(xlab, 1, " must be a character string.")
    }

    #ylab
    if(missing(ylab)) {
        ylab<-"default"
    } else {
        #length must be 
        if(diversity == FALSE) {
            check.length(ylab, 1, " must be a character string.")
        } else {
            if(length(ylab) > 2) stop("ylab can have maximum two elements.")
        }
    }

    #col
    #if default, is ok
    if(missing(col)) {
        col<-"default"
    } else {
        check.class(col, "character", " must be a character string.")
    }

    #ylim
    if(missing(ylim)) {
        ylim<-"default"
    } else {
        check.class(ylim, "numeric")
        check.length(ylim, 2, " must be a vector of two elements.")
    }

    #discrete_type
    if(type == "discrete") {
        discrete_type_methods<-c("box", "line")
        if(all(is.na(match(discrete_type, discrete_type_methods)))) stop('type must be either "box" or "line".')
    }

    #PREPARING THE PLOT

    #summarising the data
    summarised_data<-summary.dispRity(data, CI=CI, cent.tend=cent.tend, rounding=5)

    #Check the rarefaction
    if(which.rare != "max") {
        if(length(unique(summarised_data$n)) == 1) {
            if(which.rare == "plot") {
                stop("Data is not rarefied!")
            }
            rarefaction<-FALSE
            which.rare<-"max"
            message("Data is not rarefied: rarefaction is set to FALSE.")
        }
    }
    #Rarefaction must be in summarised_data
    if(class(rarefaction) == "numeric") {
        if(is.na(match(rarefaction, unlist(summarised_data$n)))) stop(paste("No rarefaction calculated for", rarefaction, "elements."))
    }

    #Check continuous (set to discrete if only one series)
    if(which.rare != "plot") {
        if(type == "continuous") {
            if(length(unique(summarised_data$series)) == 1) {
                type <- "discrete"
                message('Only one series of data available: type is set to "discrete".')
            }
        }
    }

    #Setting the default arguments
    default_arg<-set.default(summarised_data, data$call, type, diversity, ylim, xlab, ylab, col, which.rare)
    ylim<-default_arg[[1]]
    xlab<-default_arg[[2]]
    ylab<-default_arg[[3]]
    col <-default_arg[[4]]

    #PLOTTING THE RESULTS


    #Continuous plot
    if(type == "continuous") {
        if(diversity == FALSE) {
            plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, ...)
            #plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col) ; warning("DEBUG: plot")
        } else {
            bigger_margin<-par(mar=c(4,4,4,4))
            plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, ...)
            #plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col) ; warning("DEBUG: plot")
            plot.diversity(summarised_data, which.rare, ylab=ylab, col=col, ...)
            #plot.diversity(summarised_data, which.rare, ylab=ylab, col=col) ; warning("DEBUG: plot")
            par(bigger_margin)
        }
    }

    #Discrete plots
    if(type == "discrete") {
        if(diversity == FALSE) {
            plot.discrete(summarised_data, which.rare, discrete_type, ylim, xlab, ylab, col, ...)
            #plot.discrete(summarised_data, which.rare, discrete_type, ylim, xlab, ylab, col) ; warning("DEBUG: plot")
        } else {
            bigger_margin<-par(mar=c(4,4,4,4))
            plot.discrete(summarised_data, which.rare, discrete_type, ylim, xlab, ylab, col, ...)
            #plot.discrete(summarised_data, which.rare, discrete_type, ylim, xlab, ylab, col) ; warning("DEBUG: plot")
            plot.diversity(summarised_data, which.rare, ylab=ylab, col=col, ...)
            #plot.diversity(summarised_data, which.rare, ylab=ylab, col=col) ; warning("DEBUG: plot")
            par(bigger_margin)
        }        
    }


    if(which.rare == "plot") {
        #How many rarefaction plots?
        n_plots<-length(unique(summarised_data[,1]))

        #Open the multiple plots
        op_tmp<-par(mfrow=c(ceiling(sqrt(n_plots)),round(sqrt(n_plots))))

        #Rarefaction plots
        for(nPlot in 1:n_plots) {
            tmp_summarised_data<-get.series(summarised_data, rare_level=nPlot)
            plot.rarefaction(tmp_summarised_data, which.rare, ylim, xlab, ylab, col, main=level_name, ...)
            #plot.rarefaction(tmp_summarised_data, which.rare, ylim, xlab, ylab, col, main=level_name) ; warning("DEBUG: plot")
        }

        #Done!
        par(op_tmp)
    } 

    #End
}
