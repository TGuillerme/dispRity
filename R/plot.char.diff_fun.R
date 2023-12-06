## Functions for getting the the density plot limits
get.max.x <- function(density) return(max(density$x))
get.max.y <- function(density) return(max(density$y))
get.min.x <- function(density) return(min(density$x))
get.min.y <- function(density) return(min(density$y))

## Removing columns with only NAs
select.nas <- function(column) {
    if((length(column) - length(which(is.na(column)))) <= 2) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

## Plotting density
do.plot.char.diff.density <- function(matrix, main, legend, col, xlim, ylim, legend.pos, xlab, ylab) {
    ## Removing columns with NAs
    NA_columns <- which(apply(matrix, 2, select.nas) == TRUE)
    
    if(length(NA_columns) != 0) {
        matrix <- matrix[,-NA_columns]
    }


    ## Measuring the densities
    densities <- apply(matrix, 2, stats::density, na.rm = TRUE)

    ## Getting the plot limits
    if(missing(xlim)) {
        xlim = c(min(unlist(lapply(densities, get.min.x))), max(unlist(lapply(densities, get.max.x))))
    }
    if(missing(ylim)) {
        ylim = c(min(unlist(lapply(densities, get.min.y))), max(unlist(lapply(densities, get.max.y))))
    }

    ## Measuring the cumulated density
    cum_density <- stats::density(as.numeric(matrix), na.rm = TRUE)

    ## Empty plot
    plot(1,1, col = "white", xlim = xlim, ylim = ylim, main = main, xlab = xlab, ylab = ylab, bty = "n")

    ## Adding the densities
    silent <- lapply(densities, lines, col = col[2])

    ## Adding the cumulative density
    lines(cum_density, col = col[1])

    ## Adding the legend
    if(legend[1] != FALSE) {
        legend(legend.pos, legend = legend, lty = 1, col = col)
    }
}
