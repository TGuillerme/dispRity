#default settings
set.default<-function(summarised_data, call, type, diversity, ylim, xlab, ylab, col) {

    #ylim
    if(ylim == "default") {
        #Setting the ylim to min/max -/+ 5%.
        ylim=c(min(summarised_data[,-c(1:2)])-min(summarised_data[,-c(1:2)])*0.05 , max(summarised_data[,-c(1:2)])+max(summarised_data[,-c(1:2)])*0.05)
    }

    #xlab
    if(xlab == "default") {
        #Set to time (if continuous)
        if(type == "continuous") {
            xlab<-"Time (Ma)"
        } else {
            #Else to series
            xlab<-"Series"
        }
    }

    #ylab
    if(ylab == "default") {
        #Set to call label
        ylab<-strsplit(strsplit(call, split="as: ")[[1]][2], split=" for")[[1]][1]
        if(diversity == TRUE) {
            ylab[2]<-"diversity"
        }
    }

    #col
    if(col == "default") {
        col<-"black"
        #If any CIs add, grey colours
        if(ncol(summarised_data) > 3) {
            n_CIs<-(ncol(summarised_data)-3)/2
            colfun<-colorRampPalette(c("grey", "lightgrey"))
            col<-c(col, colfun(n_CIs))
        }
    }

    return(list(ylim, xlab, ylab, col))
}


    default_arg<-set.default(is.bootstrapped, diversity, ylim, xlab, ylab, col)
    ylim<-default_arg[[1]]
    xlab<-default_arg[[2]]
    ylab<-default_arg[[3]]
    col <-default_arg[[4]]



#discrete plotting
plot.discrete<-function(summarised_data, diversity, ylim, xlab, ylab, col, ...) {
#plots discrete (i.e. boxplots) results
}

#continuous plotting
plot.continuous<-function(summarised_data, diversity, ylim, xlab, ylab, col, ...) {
#plots continuous (i.e. polygons) results
}

plot.rarefaction<-function(summarised_data, ylim, xlab, ylab, col, ...) {
#plots rarefaction curves (continuous, multiple panels if series > 1)
}