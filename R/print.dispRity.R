print.dispRity<-function(x, all=FALSE, ...) {

    #If all = TRUE, return the whole x (no summary)
    if(all == TRUE) {
        y <- x
        class(y)<-"list"
        print(y)
        
    } else {
        #Series
        if(length(x) == 3) {
            #head
            cat(paste((length(x$series)-1), x$series[1], "series for", length(x$taxa), "taxa."), "\n")
            
            #series
            #remove the method time
            x$series<-x$series[-1]
            
            if(length(x$series) == 1) {
                cat(paste(length(x$series), "unnamed series."))
            } else {
                cat("Series:\n")
                if(length(x$series) > 6) {
                    cat(paste(x$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(x$series, collapse=", "), ".", sep="")
                }
            }
        }

        #Bootstraps
        if(length(x) == 4) {
            #head
            cat(paste("Bootstrapped ordinated matrix with", length(x$taxa), "taxa."), "\n")

            #series
            if(length(x$series) == 1) {
                cat(paste(length(x$series), "unnamed series."))
            } else {
                cat("Series:\n")
                if(length(x$series) > 6) {
                    cat(paste(x$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(x$series, collapse=", "), ".", sep="")
                }
            }

            #call
            cat("\n", x$call, sep="")
        }

        #Disparity
        if(length(x) == 5) {
            #head
            cat(paste("Disparity measurements across ", length(x$series), " series for ", length(x$taxa), " taxa.", sep=""), "\n")

            #series
            if(length(x$series) == 1) {
                cat(paste(length(x$series), "unnamed series."))
            } else {
                cat("Series:\n")
                if(length(x$series) > 6) {
                    cat(paste(x$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(x$series, collapse=", "), ".", sep="")
                }
            }

            #call
            cat("\n", x$call, sep="")
        }
    }
}