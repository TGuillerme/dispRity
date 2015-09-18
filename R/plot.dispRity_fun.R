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


#Extract summary values
extract.summary<-function(summarised_data, what, which.rare="max") {
    if(what=="column") {
        return(which(summarised_data$n == max(unlist(summarised_data[[2]]))))
    } else {
        if(is.numeric(which.rare)) {
            return(unlist(summarised_data[which(summarised_data$n == which.rare), what]))
        } else {
            if(which.rare == "max") which.rare.fun<-max
            if(which.rare == "min") which.rare.fun<-min
        return(unlist(summarised_data[which(summarised_data$n == which.rare.fun(unlist(summarised_data[[2]]))), what]))
        }
    }
}



plot.diversity<-function(summarised_data, which.rare, type, ylab, col, ...) {
    #Check if ylab2 exists
    if(length(ylab) == 1) {
        ylab[[2]]<-"diversity"
    }

    #Add the lines
    par(new=TRUE)
    plot(extract.summary(summarised_data, 2, which.rare), type="l", lty=2, xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4, lty=2)
    mtext(ylab[[2]], side=4, line=2, cex=1)

}



#discrete plotting
plot.discrete<-function(summarised_data, which.rare, type_d, ylim, xlab, ylab, col, ...) {
    #How many points?
    points_n<-length(extract.summary(summarised_data, "column"))

    #dummy matrix (for getting the nice boxplots split + column names)
    dummy_mat<-matrix(1:points_n, ncol=points_n)
    colnames(dummy_mat)<-extract.summary(summarised_data, 1)

    #Empty plot
    boxplot(dummy_mat, col="white", border="white", ylim=ylim, ylab=ylab[[1]], xlab=xlab, ...)
    #boxplot(dummy_mat, col="white", border="white", ylim=ylim, ylab=ylab[[1]], xlab=xlab) ; warning("DEBUG: plot")

    #Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        #How many CIs?
        CIs_n<-(ncol(summarised_data)-3)/2

        #Set the width (default)
        width=points_n/5

        #Set the colours
        if(length(col[-1]) < CIs_n) {
            col<-set.default(summarised_data, call=1, "discrete", diversity=1, ylim=1, xlab=1, ylab=1, col="default")[[4]]
            poly_col<-col[-1]
            poly_col<-rev(poly_col)
        } else {
            poly_col<-col[-1]
            poly_col<-rev(poly_col)
        }

        #Add the CIs
        if(type_d == "box") {
            for (point in 1:points_n) {
                for(cis in 1:CIs_n) {
                    #Setting X
                    x_vals<-c(point-width/(CIs_n-cis+1.5), point+width/(CIs_n-cis+1.5), point+width/(CIs_n-cis+1.5), point-width/(CIs_n-cis+1.5))
                    #Setting Y
                    y_vals<-c(extract.summary(summarised_data, 3+cis, which.rare)[point],
                              extract.summary(summarised_data, 3+cis, which.rare)[point],
                              extract.summary(summarised_data, ncol(summarised_data)-(cis-1), which.rare)[point],
                              extract.summary(summarised_data, ncol(summarised_data)-(cis-1), which.rare)[point])
                    #Plotting the box
                    polygon(x_vals, y_vals, col=poly_col[[cis]], border=col[[1]])

                }
            }
        } else {
            for (point in 1:points_n) {
                for(cis in 1:CIs_n) {
                    #Setting X
                    x_vals<-c(point-width/(CIs_n-cis+1.5), point+width/(CIs_n-cis+1.5), point+width/(CIs_n-cis+1.5), point-width/(CIs_n-cis+1.5))
                    #Setting Y
                    y_vals<-c(extract.summary(summarised_data, 3+cis, which.rare)[point],
                              extract.summary(summarised_data, 3+cis, which.rare)[point],
                              extract.summary(summarised_data, ncol(summarised_data)-(cis-1), which.rare)[point],
                              extract.summary(summarised_data, ncol(summarised_data)-(cis-1), which.rare)[point])
                    #Plotting the box
                    lines(x=c(point, point), y=c(extract.summary(summarised_data, 3+cis, which.rare)[point], extract.summary(summarised_data, ncol(summarised_data)-(cis-1), which.rare)[point]),
                        lty=(CIs_n-cis+1), lwd=cis*1.5, col=col[[1]])
                }
            }
        }
    }

    #Add the points estimates
    points(1:points_n, extract.summary(summarised_data, 3, which.rare), col=col[[1]], pch=19)

}

#continuous plotting
plot.continuous<-function(summarised_data, which.rare, ylim, xlab, ylab, col, ...) {
    #How many points?
    points_n<-length(extract.summary(summarised_data, "column"))

    #Plot the central tendency
    plot(seq(from=1, to=points_n), extract.summary(summarised_data, 3, which.rare), type="l", ylim=ylim, col=col[[1]], xlab=xlab, ylab=ylab[[1]], ...)
    #plot(seq(from=1, to=points_n), extract.summary(summarised_data, 3), type="l", ylim=ylim, col=col[[1]], xlab=xlab, ylab=ylab[[1]]) ; warning("DEBUG: plot")

    #Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        #How many CIs?
        CIs_n<-(ncol(summarised_data)-3)/2

        #Set the colours
        if(length(col[-1]) < CIs_n) {
            col<-set.default(summarised_data, call=1, "continuous", diversity=1, ylim=1, xlab=1, ylab=1, col="default")[[4]]
            poly_col<-col[-1]
            poly_col<-rev(poly_col)
        } else {
            poly_col<-col[-1]
            poly_col<-rev(poly_col)
        }

        #Add the polygons
        for (cis in 1:CIs_n) {
            x_vals<-c(1:points_n, points_n:1)
            y_vals<-c(extract.summary(summarised_data, 3+cis, which.rare), rev(extract.summary(summarised_data, ncol(summarised_data)-(cis-1), which.rare)))
            polygon(x_vals, y_vals, col=poly_col[[cis]], border="NA")
            ####
            # ADD A DENSITY OPTION!
            ###
        }

        #Add the central tendency on top
        lines(seq(from=1, to=points_n), extract.summary(summarised_data, 3, which.rare), lty=1, col=col[[1]])
    }
}


#rarefaction plottings
plot.rarefaction<-function(summarised_data, ylim, xlab, ylab, col, ...) {
#plots rarefaction curves (continuous, multiple panels if series > 1)
}
