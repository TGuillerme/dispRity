#Inspired from ape::print.phylo

print.dispRity<-function(x, ...) {
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


print.BSdispRity<-function(x, ...) {
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