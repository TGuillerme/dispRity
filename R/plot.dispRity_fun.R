## default settings
set.default <- function(summarised_data, data, elements, ylim, xlab, ylab, col, rarefaction) {

    ## ylim
    if(ylim[[1]] == "default") {
        ## Setting the ylim to min/max -/+ 5%.
        ylim = c(min(summarised_data[,-c(1:2)], na.rm = TRUE) - min(summarised_data[,-c(1:2)], na.rm = TRUE) * 0.02 , max(summarised_data[,-c(1:2)], na.rm = TRUE) + max(summarised_data[,-c(1:2)], na.rm = TRUE) * 0.02)
    }

    ## xlab
    if(xlab == "default") {
        if(rarefaction == "plot") {
            xlab <- "Elements"
        } else {
            xlab <- "Series"
        }
    }
    
    ## ylab
    if(ylab[[1]] == "default") {
        ylab <- as.character(data$call$disparity$metrics)
        if(elements == TRUE) {
            ylab[2] <- "Elements"
        }
    }

    ## col
    if(col[[1]] == "default") {
        col <- "black"
        ## If any quantiles add, grey colours
        if(ncol(summarised_data) > 3) {
            n_quantiles <- (ncol(summarised_data)-4)/2
            colfun <- colorRampPalette(c("grey", "lightgrey"))
            col <- c(col, colfun(n_quantiles))
        }
    }

    return(list(ylim, xlab, ylab, col))
}

## Extract specific summary values
### summarised_data is a summary data table
### what is the column of the table (or, if "rows", the rows numbers)
### rarefaction is the rarefaction value (FALSE == none)
extract.from.summary <- function(summarised_data, what, rarefaction = FALSE) {
    ## No rarefaction level
    if(rarefaction == FALSE) {
        if(what != "rows") {
            return(summarised_data[which(!is.na(summarised_data$obs)), what])
        } else {
            return(which(!is.na(summarised_data$obs)))
        }
    } else {
        ## Rarefaction level
        if(what != "rows") {
            return(summarised_data[which(summarised_data$n == rarefaction), what])
        } else {
            return(which(summarised_data$n == rarefaction))
        }
    }
}

## Extracting summary values for a specific series
get.series<-function(summarised_data, rare_level) {
    output <- summarised_data[which(as.factor(unlist(summarised_data[,1])) == unique(unlist(summarised_data[,1]))[rare_level]),]
    level_name <- unique(unlist(summarised_data[,1]))[rare_level]
    return(list(output, level_name))
}

## Plotting elements
plot.elements<-function(summarised_data, rarefaction, type, ylab, col, div.log, ...) {
    ## Check if ylab2 exists
    if(length(ylab) == 1) {
        ylab[[2]] <- "Elements"
    }

    ## Add the lines
    if(type == "continuous") {
        ## Continuous (straightforward)
        if(div.log == FALSE) {
            plot(extract.from.summary(summarised_data, 2, rarefaction), type = "l", lty = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        } else {
            plot(log(extract.from.summary(summarised_data, 2, rarefaction)), type = "l", lty = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        }
    } else {
        ## Creating the dummy data table
        points_n <- length(unique(summarised_data$series))
        dummy_mat <- matrix(extract.from.summary(summarised_data, 2, rarefaction), ncol = points_n)
        colnames(dummy_mat) <- extract.from.summary(summarised_data, 1)
        if(div.log == FALSE) {
            boxplot(dummy_mat, xaxt = "n", yaxt = "n", xlab = "", ylab = "", boxwex = 0.5/points_n, lty = 2)
        } else {
            boxplot(log(dummy_mat), xaxt = "n", yaxt = "n", xlab = "", ylab = "", boxwex = 0.5/points_n, lty = 2)
        }

    }
    ## lines(extract.from.summary(summarised_data, 2, rarefaction), lty=2)
    axis(4, lty = 2)
    mtext(ylab[[2]], side = 4, line = 2)
}

## discrete plotting
plot.discrete <- function(summarised_data, rarefaction, type, ylim, xlab, ylab, col, observed, add, density, ...) {

    ## How many points?
    points_n <- length(unique(summarised_data$series))

    ## dummy matrix (for getting the nice boxplots split + column names)
    dummy_mat <- matrix(1:points_n, ncol = points_n)
    colnames(dummy_mat) <- extract.from.summary(summarised_data, 1)

    ## Empty plot
    if(add == FALSE) {
        boxplot(dummy_mat, col = "white", border = "white", ylim = ylim, ylab = ylab[[1]], xlab = xlab, boxwex = 0.001, ...)
        ## boxplot(dummy_mat, col = "white", border = "white", ylim = ylim, ylab = ylab[[1]], xlab = xlab, boxwex = 0.001) ; warning("DEBUG: plot")
    }

    ## Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        ## How many quantiles?
        quantiles_n <- (ncol(summarised_data)-4)/2

        ## Set the width (default)
        width = points_n/(points_n*2)

        ## Set the colours
        if(length(col[-1]) < quantiles_n) {
            col_tmp <- set.default(summarised_data, call = 1, elements = 1, ylim = 1, xlab = 1, ylab = 1, col = "default")[[4]]
            poly_col <- col_tmp[-1]
            poly_col <- rev(poly_col)
        } else {
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        }

        ## Add the quantiles
        if(type == "polygon") {
            for (point in 1:points_n) {
                for(cis in 1:quantiles_n) {
                    ## Setting X
                    x_vals <- c(point-width/(quantiles_n-cis+1.5), point+width/(quantiles_n-cis+1.5), point+width/(quantiles_n-cis+1.5), point-width/(quantiles_n-cis+1.5))
                    ## Setting Y
                    y_vals <- c(extract.from.summary(summarised_data, 4+cis, rarefaction)[point],
                              extract.from.summary(summarised_data, 4+cis, rarefaction)[point],
                              extract.from.summary(summarised_data, ncol(summarised_data)-(cis-1), rarefaction)[point],
                              extract.from.summary(summarised_data, ncol(summarised_data)-(cis-1), rarefaction)[point])
                    ## Plotting the box
                    polygon(x_vals, y_vals, col = poly_col[[cis]], border = col[[1]], density)

                }
            }
        }
        if(type == "lines") {
            for (point in 1:points_n) {
                for(cis in 1:quantiles_n) {
                    ## Setting Y
                    y_vals<-c(extract.from.summary(summarised_data, 4+cis, rarefaction)[point],
                              extract.from.summary(summarised_data, ncol(summarised_data)-(cis-1), rarefaction)[point])
                    ## Plotting the box
                    lines(x = c(point, point), y = y_vals, lty = (quantiles_n-cis+1), lwd = cis*1.5, col = col[[1]])

                }
            }
        }
    }

    ## Add the points estimates
    points(1:points_n, extract.from.summary(summarised_data, 4, rarefaction), col = col[[1]], pch = 19)

    if(observed == TRUE) {
        if(any(!is.na(extract.from.summary(summarised_data, 3, rarefaction))))
        ## Add the points observed (if existing)
        if(type == "polygon") {
            for (point in 1:points_n) {
                x_coord <- c(point-width/(quantiles_n-2+1.5), point+width/(quantiles_n-2+1.5))
                y_coord <- rep(extract.from.summary(summarised_data, 3, rarefaction)[point], 2)
                lines(x_coord, y_coord, col = col[[1]], lty = 3, lwd = 2)
            }
        } else {
            for (point in 1:points_n) {
                y_coord <- extract.from.summary(summarised_data, 3, rarefaction)[point]
                points(point, y_coord, col = col[[1]], pch = 4, cex = 1.5)
            }
        }
    }

    ##  Save parameters
    return(par())
}

## continuous plotting
plot.continuous<-function(summarised_data, rarefaction, ylim, xlab, ylab, col, time_slicing, observed, add, density, ...) {
    ## How many points?
    points_n <- length(unique(summarised_data$series))

    ## Plot the central tendency
    if(add == FALSE) {
        if(time_slicing[1] == FALSE) {
            ## Plot with standard xaxis
            plot(seq(from = 1, to = points_n), extract.from.summary(summarised_data, 4, rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]], ...)
            ## plot(seq(from = 1, to = points_n), extract.from.summary(summarised_data, 4, rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]]) ; warning("DEBUG: plot")
        } else {
            plot(seq(from = 1, to = points_n), extract.from.summary(summarised_data, 4, rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]], xaxt = "n", ...)
            ## plot(seq(from = 1, to = points_n), extract.from.summary(summarised_data, 4, rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]], xaxt = "n") ; warning("DEBUG: plot")
            axis(1, 1:points_n, time_slicing)
        }
    } else {
        lines(seq(from = 1, to = points_n), extract.from.summary(summarised_data, 4, rarefaction), col = col[[1]])
    }

    ## Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        ## How many quantiles?
        quantiles_n <- (ncol(summarised_data)-4)/2

        ## Set the colours
        if(length(col[-1]) < quantiles_n) {
            col <- set.default(summarised_data, call = 1, elements = 1, ylim = 1, xlab = 1, ylab = 1, col = "default", rarefaction)[[4]]
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        } else {
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        }

        ## Add the polygons
        for (cis in 1:quantiles_n) {
            x_vals <- c(1:points_n, points_n:1)
            y_vals <- c(extract.from.summary(summarised_data, 4+cis, rarefaction), rev(extract.from.summary(summarised_data, ncol(summarised_data)-(cis-1), rarefaction)))
            polygon(x_vals, y_vals, col = poly_col[[cis]], border = "NA", density)
            ## ## ## ## 
            ##  ADD A DENSITY OPTION!
            ## ## ## 
        }

        ## Add the central tendency on top
        lines(seq(from = 1, to = points_n), extract.from.summary(summarised_data, 4, rarefaction), lty = 1, col = col[[1]])

        ## Add the observed values on top
        if(observed == TRUE) {
            for (point in 1:points_n) {
                y_coord <- extract.from.summary(summarised_data, 3, rarefaction)[point]
                points(point, y_coord, col = col[[1]], pch = 4, cex = 1.5)
            }
        }
    }

    ##  Save parameters
    return(par())
}


## rarefaction plottings
plot.rarefaction<-function(summarised_data, rarefaction, ylim, xlab, ylab, col, ...) {
## plots rarefaction curves (continuous, multiple panels if series > 1)

    plot(summarised_data[,3], type = "l", xlab = xlab, ylab = ylab[[1]], col = col[[1]], ylim = ylim, ...)
    ## plot(summarised_data[,3], type = "l", xlab = xlab, ylab = ylab[[1]], col = col[[1]], ylim = ylim) ; warning("DEBUG: plot")

    ## Add the quantiles
    ## Check if bootstrapped
    if(ncol(summarised_data) > 3) {
        ## How many quantiles?
        quantiles_n <- (ncol(summarised_data)-4)/2

        ## Set the colours
        if(length(col[-1]) < quantiles_n) {
            col <- set.default(summarised_data, call = 1, elements = 1, ylim = 1, xlab = 1, ylab = 1, col = "default", rarefaction)[[4]]
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        } else {
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        }

        ## Add the quantile lines (from inner to outer quantiles)
        for (cis in 1:quantiles_n) {
            ## lower quantile
            lines(summarised_data[, 4+cis], lty = (quantiles_n+2-cis))## , col = poly_col[cis])
            ## upper quantile
            lines(summarised_data[, ncol(summarised_data) - (cis-1)], lty = (quantiles_n+2-cis))## , col = poly_col[cis])
        }

        ## Add the central tendency
        lines(summarised_data[, 4], lty = 1)## , col = col[1])

    }

    ##  Save parameters
    return(par())
}

## Transposing data for boxploting (taking functions from summary.dispRity)
transpose.box <- function(data, rarefaction) {
    ## Getting the bootstrapped results
    BSresults <- data$disparity$bootstrapped
    ## Unlisting the results
    BSresults_unlist <- unlist(recursive.unlist(BSresults), recursive=FALSE)
    ## Get the rarefaction multiplier
    multiplier <- unlist(lapply(BSresults, length))
    ## Create the (dummy) table results
    results_table <- data.frame(cbind(rep(data$series, multiplier), diversity.count(data$data$bootstraps)), stringsAsFactors=FALSE)

    ## Get the vector of list elements to print out
    if(is.numeric(rarefaction)) {
        list_elements_out <- which(as.numeric(results_table[,2]) == rarefaction)
    } else {
        if(rarefaction == "max") rarefaction.fun <- max
        if(rarefaction == "min") rarefaction.fun <- min
        list_elements_out <- NULL
        for(series in 1:length(multiplier)) {
            ## Selecting the right series
            right_series <- which(results_table[,1] == names(multiplier)[series])
            ## Selecting the max/min from there
            list_elements_out[series] <- rarefaction.fun(right_series)
        }
    }

    ## Extract the list
    list_out <- BSresults_unlist[list_elements_out]
    ## Add the names
    names(list_out) <- names(multiplier)
    return(list_out)
}

## Adding a line
add.line <- function(xs, ys, lines.args) {
    if(!is.null(lines.args)) {
        ## Adding the x,y coordinates
        lines.args$x <- xs ; lines.args$y <- ys
        do.call(lines, lines.args)
    } else {
        lines(xs, ys)
    }
}

## Adding significance tokens
significance.token <- function(xs, ys, p.value, token.args) {
    if(p.value < 0.1) {
        ## Selecting the token
        if(p.value < 0.1) token <- "."
        if(p.value < 0.05) token <- "*"
        if(p.value < 0.01) token <- "**"
        if(p.value < 0.001) token <- "***"
        ## Default plotting
        if(is.null(token.args)) {
            text(x = sum(xs)/2, y = max(ys)+max(ys)*0.05, token)
        } else {
        ## Plotting with arguments
            token.args$labels <- token
            token.args$x <- sum(xs)/2
            if(any(names(token.args) == "float")) {
                token.args$y <- max(ys)+max(ys)*token.args$float
                token.args$float <- NULL
            } else {
                token.args$y <- max(ys)+max(ys)*0.05
            }
            do.call(text, token.args)
        }
    }
}

## Getting the two coordinates of the intercepts (intercept0 and intercept predicted)
get.intercept.coords <- function(results_out, model_number, is.distribution, significance) {
    if(is.distribution != TRUE) {
        ## Get the first y coordinate (first intercept)
        y1 <- results_out$Intercept[model_number, 1]
        ## Test if intercept0 is significant
        if(model_number == 1) {
            if(results_out$Intercept[model_number, 4] > 0.05) {
                y1 <- 0
            }
        }

        ## Get the second y coordinate (second intercept)
        if(model_number < nrow(results_out$Intercept)) {
            ## Intercept already estimated
            y2 <- results_out$Intercept[model_number+1, 1]
        } else {
            ## Estimate the intercept
            if(results_out$Slopes[model_number, 4] < 0.05) {
                slope <- results_out$Slopes[model_number, 1]
            } else {
                slope <- 0
            }
            y2 <- intercept.estimate(results_out$Intercepts[model_number,1], slope)
        }
    } else {
        ## Get the first y coordinate
        if(model_number == 1) {
            ## Get the initial (estimated) intercept if significant
            if(results_out$Intercept$Initial[4, significance] < 0.05) {
                y1 <- unlist(results_out$Intercept$Initial[1, significance])
            } else {
                y1 <- 0
            }
        } else {
            ## Get the predicted intercept
            y1 <- unlist(results_out$Intercepts$Predicted[model_number-1, significance])
        }

        ## Get the second y coordinate (second intercept)
        if(model_number-1 < nrow(results_out$Intercept$Predicted)) {
            ## Intercept already estimated
            y2 <- unlist(results_out$Intercepts$Predicted[model_number, significance])
        } else {
            ## Estimate the intercept
            if(results_out$Slopes$`Pr(>|t|)`[model_number, significance] < 0.05) {
                slope <- unlist(results_out$Slopes$Estimate[model_number, significance])
            } else {
                slope <- 0
            }
            y2 <- intercept.estimate(unlist(results_out$Intercepts$Predicted[model_number-1, significance]), slope)
        }        
    }

    ## Return the coordinates
    return(c(y1, y2))
}

## Plotting the results of sequential tests
plot.seq.test <- function(results_out, is.distribution, significance, lines.args, token.args) {
    ## Get the number of models to plot
    if(is.distribution != TRUE) {
        n_models <- nrow(results_out$Slopes)
    } else {
        n_models <- nrow(results_out$Slopes$Estimate)
    }

    ## Loop through each model
    for(model_number in 1:n_models) {
        ## Getting x,y coordinates for one model
        x_coords <- c(model_number, model_number+1)
        y_coords <- get.intercept.coords(results_out, model_number=model_number, is.distribution, significance)

        ## Plotting the line
        add.line(x_coords, y_coords, lines.args)

        ## Add significance (if necessary)
        if(is.distribution != TRUE) {
            p_value <- results_out$Slope[model_number, 4]
        } else {
            p_value <- results_out$Slope$`Pr(>|t|)`[model_number, significance]
            ## get p_value
        }

        significance.token(x_coords, y_coords, p_value, token.args)
    }
}


## The following is a modified version of plot.randtest from ade4 v1.4-3
plot.randtest <- function (data_sub, nclass = 10, coeff = 1, ...) {
    obs <- data_sub$obs
    sim <- data_sub$sim
    r0 <- c(sim, obs)
    l0 <- max(sim) - min(sim)
    w0 <- l0/(log(length(sim), base = 2) + 1)
    w0 <- w0 * coeff
    xlim0 <- range(r0) + c(-w0, w0)
    h0 <- hist(sim, plot = FALSE, nclass = nclass)
    y0 <- max(h0$counts)

    hist(sim, plot = TRUE, nclass = nclass, xlim = xlim0, col = grey(0.8), 
        ...)
    lines(c(obs, obs), c(y0/2, 0))
    points(obs, y0/2, pch = 18, cex = 2)

##     hist(sim, plot = TRUE, nclass = nclass, xlim = xlim0, col = grey(0.8)) ; warning("DEBUG plot.randtest")
    legend("topleft", bty = "n", legend = c("p-value", round(data_sub$pvalue, 5)), cex = 0.7, adj = 0.2)

}