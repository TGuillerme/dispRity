#default settings
set.default<-function(summarised_data, call, type, diversity, ylim, xlab, ylab, col, which.rare) {

    #ylim
    if(ylim[[1]] == "default") {
        #Setting the ylim to min/max -/+ 5%.
        ylim=c(min(summarised_data[,-c(1:2)])-min(summarised_data[,-c(1:2)])*0.05 , max(summarised_data[,-c(1:2)])+max(summarised_data[,-c(1:2)])*0.05)
    }

    #xlab
    if(xlab == "default") {
        if(which.rare == "plot") {
            xlab<-"Elements"
        } else {
            xlab<-"Series"
        }
    }
    
    #ylab
    if(ylab[[1]] == "default") {
        #Set to call label
        ylab<-strsplit(strsplit(call, split="as: ")[[1]][2], split=" for")[[1]][1]
        if(diversity == TRUE) {
            ylab[2]<-"Diversity"
        }
    }

    #col
    if(col[[1]] == "default") {
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
    #Set up which rarefaction function
    if(is.numeric(which.rare)) {
        rare.fun<-FALSE
    } else {
        rare.fun<-TRUE
        if(which.rare == "max") which.rare.fun<-max
        if(which.rare == "min") which.rare.fun<-min
    }

    #Extracting specific a level of rarefaction
    if(rare.fun == FALSE) {
        which_rows<-which(unlist(summarised_data$n) == which.rare)
    }

    #Extracting the minimum or maximum rarefaction
    if(rare.fun == TRUE) {
        #Get the different levels
        series_levels<-unique(summarised_data$series)
        #Get the right rarefaction value
        which_rows<-NULL
        for(series in 1:length(series_levels)) {
            #Selecting the right series
            right_series<-which(unlist(summarised_data$series) == series_levels[series])
            #Selecting the max/min from there
            which_rows[series]<-which.rare.fun(right_series)
        }
    }

    #Return the right values
    if(what == "rows") {
        return(unlist(which_rows))
    } else {
        return(unlist(summarised_data[which_rows, what]))
    }
}

#Extracting summary values for a specific series
get.series<-function(summarised_data, rare_level) {
    output<-summarised_data[which(as.factor(unlist(summarised_data[,1])) == unique(unlist(summarised_data[,1]))[rare_level]),]
    level_name<<-unique(unlist(summarised_data[,1]))[rare_level]
    return(output)
}


plot.diversity<-function(summarised_data, which.rare, type, ylab, col, div.log, ...) {
    #Check if ylab2 exists
    if(length(ylab) == 1) {
        ylab[[2]]<-"Diversity"
    }

    #Add the lines
    if(type == "continuous") {
        #Continuous (straightforward)
        if(div.log == FALSE) {
            plot(extract.summary(summarised_data, 2, which.rare), type="l", lty=2, xaxt="n",yaxt="n",xlab="",ylab="")
        } else {
            plot(log(extract.summary(summarised_data, 2, which.rare)), type="l", lty=2, xaxt="n",yaxt="n",xlab="",ylab="")
        }
    } else {
        #Creating the dummy data table
        points_n<-length(unique(summarised_data$series))
        dummy_mat<-matrix(extract.summary(summarised_data, 2, which.rare), ncol=points_n)
        colnames(dummy_mat)<-extract.summary(summarised_data, 1)
        if(div.log == FALSE) {
            boxplot(dummy_mat,  xaxt="n",yaxt="n",xlab="",ylab="", boxwex=0.5/points_n, lty=2)
        } else {
            boxplot(log(dummy_mat),  xaxt="n",yaxt="n",xlab="",ylab="", boxwex=0.5/points_n, lty=2)
        }

    }
    #lines(extract.summary(summarised_data, 2, which.rare), lty=2)re
    axis(4, lty=2)
    mtext(ylab[[2]], side=4, line=2)
}

#discrete plotting
plot.discrete<-function(summarised_data, which.rare, type_d, ylim, xlab, ylab, col, ...) {
    #How many points?
    points_n<-length(unique(summarised_data$series))

    #dummy matrix (for getting the nice boxplots split + column names)
    dummy_mat<-matrix(1:points_n, ncol=points_n)
    colnames(dummy_mat)<-extract.summary(summarised_data, 1)

    #Empty plot
    boxplot(dummy_mat, col="white", border="white", ylim=ylim, ylab=ylab[[1]], xlab=xlab, boxwex=0.001, ...)
    #boxplot(dummy_mat, col="white", border="white", ylim=ylim, ylab=ylab[[1]], xlab=xlab, boxwex=0.001) ; warning("DEBUG: plot")

    #Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        #How many CIs?
        CIs_n<-(ncol(summarised_data)-3)/2

        #Set the width (default)
        width=points_n/10

        #Set the colours
        if(length(col[-1]) < CIs_n) {
            col_tmp<-set.default(summarised_data, call=1, "discrete", diversity=1, ylim=1, xlab=1, ylab=1, col="default", which.rare)[[4]]
            poly_col<-col_tmp[-1]
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
plot.continuous<-function(summarised_data, which.rare, ylim, xlab, ylab, col, time_slicing, ...) {
    #How many points?
    points_n<-length(unique(summarised_data$series))

    #Plot the central tendency
    if(time_slicing[1] == FALSE) {
        #Plot with standard xaxis
        plot(seq(from=1, to=points_n), extract.summary(summarised_data, 3, which.rare), type="l", ylim=ylim, col=col[[1]], xlab=xlab, ylab=ylab[[1]], ...)
        #plot(seq(from=1, to=points_n), extract.summary(summarised_data, 3, which.rare), type="l", ylim=ylim, col=col[[1]], xlab=xlab, ylab=ylab[[1]]) ; warning("DEBUG: plot")
    } else {
        plot(seq(from=1, to=points_n), extract.summary(summarised_data, 3, which.rare), type="l", ylim=ylim, col=col[[1]], xlab=xlab, ylab=ylab[[1]], xaxt = "n", ...)
        #plot(seq(from=1, to=points_n), extract.summary(summarised_data, 3, which.rare), type="l", ylim=ylim, col=col[[1]], xlab=xlab, ylab=ylab[[1]], xaxt = "n") ; warning("DEBUG: plot")
        axis(1, 1:points_n, time_slicing)
    }

    #Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        #How many CIs?
        CIs_n<-(ncol(summarised_data)-3)/2

        #Set the colours
        if(length(col[-1]) < CIs_n) {
            col<-set.default(summarised_data, call=1, "continuous", diversity=1, ylim=1, xlab=1, ylab=1, col="default", which.rare)[[4]]
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
plot.rarefaction<-function(summarised_data, which.rare, ylim, xlab, ylab, col, ...) {
#plots rarefaction curves (continuous, multiple panels if series > 1)

    plot(summarised_data[,3], type="l", xlab=xlab, ylab=ylab[[1]], col=col[[1]], ylim=ylim, ...)
    #plot(summarised_data[,3], type="l", xlab=xlab, ylab=ylab[[1]], col=col[[1]], ylim=ylim) ; warning("DEBUG: plot")

    #Add the CIs
    #Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        #How many CIs?
        CIs_n<-(ncol(summarised_data)-3)/2

        #Set the colours
        if(length(col[-1]) < CIs_n) {
            col<-set.default(summarised_data, call=1, "continuous", diversity=1, ylim=1, xlab=1, ylab=1, col="default", which.rare)[[4]]
            poly_col<-col[-1]
            poly_col<-rev(poly_col)
        } else {
            poly_col<-col[-1]
            poly_col<-rev(poly_col)
        }

        #Add the CI lines (from inner to outer CIs)
        for (cis in 1:CIs_n) {
            #lower CI
            lines(summarised_data[,(3+CIs_n-(cis-1))], lty=(1+cis))
            #upper CI
            lines(summarised_data[,3+CIs_n+cis], lty=(1+cis))
        }

    }
}
