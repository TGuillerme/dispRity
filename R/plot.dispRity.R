plot.dispRity<-function(data, type="continuous", CI=c(50,95), cent.tend=mean, rarefaction=FALSE, diversity=FALSE, ylim, xlab, ylab, col, type_d="box", ...){
    #cex.xaxis?
    #add=FALSE?

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

    #type_d
    if(type_d == "discrete") {
        type_d_methods<-c("box", "line")
        if(all(is.na(match(type_d, type_d_methods)))) stop('type must be either "box" or "line".')
    }

    #PREPARING THE PLOT

    #summarising the data
    summarised_data<-summary.dispRity(data, CI=CI, cent.tend=cent.tend, rounding=5)

    #Check the rarefaction
    if(length(unique(summarised_data$n)) == 1) {
        rarefaction<-FALSE
        warning("Data is not rarefied: rarefaction is set to FALSE.")
    }

    #Check continuous (set to discrete if only one series)
    if(length(unique(summarised_data$series)) == 1) {
        type <- "discrete"
        warning('Only one series of data available: type is set to "discrete".')
    }

    #Setting the default arguments
    default_arg<-set.default(summarised_data, data$call, type, diversity, ylim, xlab, ylab, col)
    ylim<-default_arg[[1]]
    xlab<-default_arg[[2]]
    ylab<-default_arg[[3]]
    col <-default_arg[[4]]

    #PLOTTING THE RESULTS

    #Continuous plot
    if(type == "continuous") {
        if(diversity == FALSE) {
            plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, ...)
        } else {
            bigger_margin<-par(mar=c(4,4,4,4))
            plot.continuous(summarised_data, which.rare, ylim, xlab, ylab, col, ...)
            plot.diversity(summarised_data, which.rare, ylab, col, ...)
            par(bigger_margin)
        }
    }

    #Discrete plots
    if(type == "discrete") {
        if(diversity == FALSE) {
            plot.discrete(summarised_data, which.rare, type_d, ylim, xlab, ylab, col, ...)
        } else {
            bigger_margin<-par(mar=c(4,4,4,4))
            plot.discrete(summarised_data, which.rare, type_d, ylim, xlab, ylab, col, ...)
            plot.diversity(summarised_data, which.rare, ylab, col, ...)
            par(bigger_margin)
        }        
    }


    if(rarefaction == TRUE) {
        plot.rarefaction()
    } else {
        if(type == "discrete") {
            plot.discrete()
        } else {
            plot.continuous()
        }
    }


    #End
}


        # if(rarefaction == TRUE) {
        #     #ylim
        #     if(missing(ylim)) {
        #         ylim=c(min(disparity_data[,CI_min]),max(disparity_data[,CI_max]))
        #     }
        #     #Plotting the rarefaction curve
        #     plot(disparity_data[,1], disparity_data[,measure_col], type='l', ylim=ylim , ...)
        #     #Add the CIs
        #     for (n in 1:(CI_length/2)) {
        #         #Add both lines
        #         lines(disparity_data[,1], disparity_data[,CI_pairs[n,1]], type='l', lty=lty_list[n+1])
        #         lines(disparity_data[,1], disparity_data[,CI_pairs[n,2]], type='l', lty=lty_list[n+1])
        #     }