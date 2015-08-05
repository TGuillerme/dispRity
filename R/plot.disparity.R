##########################
#Plotting disparity results
##########################
#Plots the disparity results
#v0.2.2
##########################
#SYNTAX :
#<disparity> disparity data
#<measure> the name of the column containing the disparity measurement. If set to 'default' the measure will be the first measure (second column) of the table.
#<rarefaction> whether to plot the rarefaction results or not
#<diversity> optional. Must be a vector of the same length as disparity_data.
#<add> optional. Whether to add the a previous called graph
#<ylim> optional. A set of values for the y axis limit
#<y2lab> optional. A value for the label of the second y axis (diversity)
#<cex.xaxis> optional. A value for the size of the font of the x axis
##########################
#----
#guillert(at)tcd.ie 15/05/2015
##########################

plot.disparity<-function(disparity_data, measure="Cent.dist", rarefaction=FALSE, xlab="default", ylab="default", col="default", diversity, add=FALSE, ylim, y2lab, cex.xaxis, ...){
    #SANITIZING
    #Disparity
    check.class(disparity_data, 'data.frame')
    if(length(disparity_data) < 4) {
        stop("Disparity data.frame must have in the following order:\na 'rarefaction' column, a 'measurement' column and at least two 'Confidence Interval' columns.\nUse the disparity() function to generate the proper formatted data.frame.")
    }

    #Measure
    check.class(measure, 'character', " must be 'default' or one of the names of the columns in the disparity data.frame.")
    check.length(measure, 1, " must be 'default' or one of the names of the columns in the disparity data.frame.", errorif=FALSE)
    #Get the right column number
    measure_col<-grep(measure, colnames(disparity_data))
    if(length(measure_col) != 1) {
        stop("measure column not found in disparity_data.\nUse the disparity() function to generate the proper formatted data.frame.")
    }

    #Get the Confidence intervals columns
    measure_col_tmp<-measure_col
    while(length(grep("%", colnames(disparity_data)[(measure_col_tmp+1)]))==1) {
        measure_col_tmp<-measure_col_tmp+1
    }
    #Get the column position for the different CI_values
    CI_length<-(measure_col_tmp-measure_col)
    CI_min<-measure_col+1
    CI_max<-measure_col+CI_length
    #Get the eventual intermediate CIs
    #number of CI values
    n_CI<-CI_length/2
    #extracting all the CI column number pairs in a table
    CI_pairs<-matrix(nrow=n_CI, ncol=2)
    for(n in 1:n_CI) {
        CI_pairs[n,1]<-CI_min+n-1
        CI_pairs[n,2]<-CI_max-n+1
    }
    #Setting the line types for the CIs
    lty_list<-c(44,33,22,21,12)

    #Rarefaction
    check.class(rarefaction, 'logical')
    #Check if rarefaction data is available
    options(warn=-1)
    if(rarefaction == TRUE & is.na(disparity_data[,1])) {
        stop("No rarefaction data available.\nUse the disparity() function to generate the proper formatted data.frame with the option 'rarefaction=TRUE'.")
    }
    #If no rarefaction, check if disparity data is > 1
    if(rarefaction == FALSE & nrow(disparity_data) < 2) {
        warning("Only one disparity point is available.")
    }
    options(warn=0)

    #xlab
    check.class(xlab, "character", " must be a character string.")
    check.length(xlab, 1, " must be a character string.", errorif=FALSE)
    #ylab
    check.class(ylab, "character", " must be a character string.")
    check.length(ylab, 1, " must be a character string.", errorif=FALSE)
    #col
    check.class(col, "character", " must be a character string.")

    #diversity
    if(missing(diversity)) {
        plot.diversity<-FALSE
    } else {
        plot.diversity<-TRUE
        #check.class(diversity, "integer", " must be a numeric vector of the same number of rows as disparity_data.")
        check.length(diversity, nrow(disparity_data), " must be a numeric vector of the same number of rows as disparity_data.")
    }

    #add
    check.class(add, "logical")

    #y2lab
    if(missing(y2lab)) {
        y2lab<-"Diversity"
    } else {
        check.class(y2lab, "character", " must be a character string.")
        check.length(y2lab, 1, " must be a character string.", errorif=FALSE)
    }

    #cex.xaxis
    if(missing(cex.xaxis)) {
        cex.xaxis<-1
    } else {
        check.class(cex.xaxis, 'numeric')
        check.length(cex.xaxis, 1, ' must be a single numeric value.', errorif=FALSE)
    }

    #PLOTTING THE DISPARITY RESULTS
    if(add == FALSE) {
        if(rarefaction == TRUE) {
            #ylim
            if(missing(ylim)) {
                ylim=c(min(disparity_data[,CI_min]),max(disparity_data[,CI_max]))
            }
            #Plotting the rarefaction curve
            plot(disparity_data[,1], disparity_data[,measure_col], type='l', ylim=ylim , ...)
            #Add the CIs
            for (n in 1:(CI_length/2)) {
                #Add both lines
                lines(disparity_data[,1], disparity_data[,CI_pairs[n,1]], type='l', lty=lty_list[n+1])
                lines(disparity_data[,1], disparity_data[,CI_pairs[n,2]], type='l', lty=lty_list[n+1])
            }

        } else {
            #Plotting the disparity curve
            if(nrow(disparity_data) == 1) {
                #ylim
                if(missing(ylim)) {
                    ylim=c(min(disparity_data[,CI_min]),max(disparity_data[,CI_max]))
                }
                #If only one data point is available, do box plot style
                plot(1,1, xlab='', ylab='', ylim=ylim, type='n', xaxt='n')
                points(1,disparity_data[,measure_col], pch=19)
                #line types for this one
                lty_list2<-c(44,1,1,1,1,1)
                for (n in 1:(CI_length/2)) {
                    #Add CIs lines
                    lines(c(1,1), c(disparity_data[,CI_pairs[n,1]], disparity_data[,CI_pairs[n,2]]), lwd=1+(n-1)*3, lty=lty_list2[n])
                }
            } else {
                #Setting plot options (defaults)
                #xlab
                if(xlab=="default") {
                    xlab="Time"
                }
                #ylab
                if(ylab=="default") {
                    ylab=names(disparity_data)[measure_col]
                }
                #colors
                if(col=="default") {
                    #line color
                    line_color<-"black"
                    #polygon_colors
                    polygon_colors<-c("lightgrey", "grey")
                } else {
                    #line color
                    line_color<-col[1]
                    #polygon_colors
                    polygon_colors<-col
                }

                #ylim
                if(missing(ylim)){
                    ylim=c(min(disparity_data[,CI_min]),max(disparity_data[,CI_max]))
                }

                #Plotting the curve
                plot(seq(from=1, to=nrow(disparity_data)), disparity_data[,measure_col], type='l', 
                    ylim=ylim ,col="white", ylab=ylab, xlab=xlab, xaxt='n' , ...)
                    #ylim=ylim ,col="white", ylab=ylab, xlab=xlab, xaxt='n'); warning("plotting is in debug mode")

                #X axis options
                if(any(grep("-", disparity_data[,1]))) {
                    #if axis is character (bins)
                    axis(side = 1, 1:nrow(disparity_data), disparity_data[,1], las=2, cex.axis=cex.xaxis)
                } else {
                    if(class(disparity_data[,1]) == "factor") {
                        #if axis is factors (dates)
                        axis(side=1, c(seq(from=1, to=nrow(disparity_data), by=5), nrow(disparity_data)), disparity_data[c(seq(from=1, to=nrow(disparity_data), by=5), nrow(disparity_data)),1], las=1, cex.axis=cex.xaxis)
                    } else {
                        #just print the ticks
                        axis(side = 1, 1:nrow(disparity_data), cex.axis=cex.xaxis)
                    }
                }
                #Add the polygons
                for (n in 1:(CI_length/2)) {
                    polygon(c(seq(from=1, to=nrow(disparity_data)), seq(from=nrow(disparity_data), to=1)),
                        c(disparity_data[,CI_pairs[n,1]], rev(disparity_data[,CI_pairs[n,2]])),
                        col=polygon_colors[n], density=100-(100/(CI_length/2)/2.5*n))
                }
                #ylim
                if(missing(ylim)){
                    ylim=c(min(disparity_data[,CI_min]),max(disparity_data[,CI_max]))
                }
                #Add the central tendency line
                lines(seq(from=1, to=nrow(disparity_data)), disparity_data[,measure_col], type='l', ylim=ylim, col=line_color)

                #Add the diversity (optional)
                if(plot.diversity==TRUE) {
                    par(new=TRUE)
                    plot(diversity, type="l", lty=2, xaxt="n",yaxt="n",xlab="",ylab="")
                    axis(4, lty=2)
                    mtext(y2lab,side=4, line=2.5, cex=0.8)
                }
            }
        }
    } else {

        #Adding to the existing plot
        stop("add=TRUE option in development.")
    }
}