## default settings
set.default <- function(summarised_data, data, elements, ylim, xlab, ylab, col, rarefaction, type = FALSE, is_bootstrapped) {

    ## ylim
    if(ylim[[1]] == "default") {
        ## Setting the ylim to min/max -/+ 5%.
        if(rarefaction != TRUE) {
            ylim <- c(min(summarised_data[, -c(1:2)], na.rm = TRUE) - min(summarised_data[, -c(1:2)], na.rm = TRUE) * 0.02 , max(summarised_data[, -c(1:2)], na.rm = TRUE) + max(summarised_data[, -c(1:2)], na.rm = TRUE) * 0.02)
        } else {
            ylim <- "rarefaction"
        }
    }

    ## xlab
    if(xlab == "default") {
        if(rarefaction == TRUE) {
            xlab <- "Elements"
        } else {
            xlab <- "Subsets"
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
            quantiles_n <- (ncol(summarised_data) - ifelse(is_bootstrapped, 4, 3))/2
            colfun <- grDevices::colorRampPalette(c("grey", "lightgrey"))
            col <- c(col, colfun(quantiles_n))
        }
    } else {
        if(type != "box") {
            quantiles_n <- ncol(summarised_data[, -c(1:ifelse(is_bootstrapped, 4, 3))])/2
            cols_missing <- (quantiles_n + 1) - length(col)
            if(cols_missing > 0) {
                colfun <- grDevices::colorRampPalette(c("grey", "lightgrey"))
                col <- c(col, colfun(cols_missing))
            }
        }
    }

    return(list(ylim, xlab, ylab, col))
}

## Extract specific summary values
### summarised_data is a summary data table
### what is the column of the table (or, if "rows", the rows numbers)
### rarefaction is the rarefaction value (FALSE == none)
extract.from.summary <- function(summarised_data, what, rarefaction = FALSE) {

    ## Internal function for checking true NAs
    check.na <- function(row, extract, summarised_data) {
        if(!extract[row]) {
            extract[row] <- ifelse(all(is.na(summarised_data[row,-c(1,2)])), TRUE, FALSE)
        }
        return(extract[row])
    }

    ## No rarefaction level
    if(!rarefaction) {
        ## Values to extract
        extract <- !is.na(summarised_data$obs)
        ## Check if any of the values to extract are NAs from rarefaction or from missing data
        if(any(!extract)) {
            extract <- sapply(1:nrow(summarised_data), check.na, extract, summarised_data)
        }

        if(what != "rows") {
            return(summarised_data[which(extract), what])
        } else {
            return(which(extract))
        }
    } else {
        ## Rarefaction level
        if(!(what == "rows")) {
            return(summarised_data[which(summarised_data$n == rarefaction), what])
        } else {
            return(which(summarised_data$n == rarefaction))
        }
    }
}

## discrete plotting
plot.discrete <- function(summarised_data, rarefaction, is_bootstrapped, is_distribution, type, ylim, xlab, ylab, col, observed, add, density, ...) {


    ## How many points?
    points_n <- length(unique(summarised_data$subsets))

    ## dummy matrix (for getting the nice boxplots split + column names)
    dummy_mat <- matrix(1:points_n, ncol = points_n)
    colnames(dummy_mat) <- extract.from.summary(summarised_data, 1)

    ## Empty plot
    if(add == FALSE) {
        boxplot(dummy_mat, col = "white", border = "white", ylim = ylim, ylab = ylab[[1]], xlab = xlab, boxwex = 0.001,, type = "n", ...)
        #boxplot(dummy_mat, col = "white", border = "white", ylim = ylim, ylab = ylab[[1]], xlab = xlab, boxwex = 0.001, type = "n") ; warning("DEBUG: plot")
    }

    ## Set the shift parameter (for add)
    shift = 0
    if(add) {
        ## Is the previous plot the same size?
        prev_axis <- par("xaxp")
        if(prev_axis[2] == points_n) {
            shift = 0
        } else {
            shift = 0.5
        }
    }

    ## Check if bootstrapped
    if(is_bootstrapped || is_distribution) {
        ## How many quantiles?
        quantiles_n <- (ncol(summarised_data) - ifelse(is_bootstrapped, 4, 3))/2

        ## Set the width (default)
        width <- 0.5 #points_n/(points_n*2)

        ## Set the colours
        if(length(col) < (quantiles_n + 1)) {
            cols_missing <- (quantiles_n + 1) - length(col)
            colfun <- colorRampPalette(c("grey", "lightgrey"))
            col_tmp <- c(col, colfun(cols_missing))
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
                    x_vals <- c(point-width/(quantiles_n - cis + 1.5), point+width/(quantiles_n - cis + 1.5), point+width/(quantiles_n - cis + 1.5), point-width/(quantiles_n - cis + 1.5)) + shift
                    ## Setting Y
                    y_vals <- c(rep(extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3) + cis, rarefaction)[point], 2),
                              rep(extract.from.summary(summarised_data, (ifelse(is_bootstrapped, 4, 3) + quantiles_n*2) - (cis - 1), rarefaction)[point], 2)
                              )
                    ## Plotting the box
                    polygon(x_vals, y_vals, col = poly_col[[cis]], border = col[[1]], density)

                }
            }
        }
        if(type == "line") {
            for (point in 1:points_n) {
                for(cis in 1:quantiles_n) {
                    ## Setting Y
                    y_vals<-c(extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3) + cis, rarefaction)[point],
                              extract.from.summary(summarised_data, (ifelse(is_bootstrapped, 4, 3) + quantiles_n*2) - (cis - 1), rarefaction)[point])
                    ## Plotting the box
                    lines(x = rep((point + shift), 2), y = y_vals, lty = (quantiles_n - cis + 1), lwd = cis * 1.5, col = col[[1]])

                }
            }
        }
        ## Add the points estimates
        points(1:points_n + shift, extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), col = col[[1]], pch = 19)
    } else {
        
        ## Add the points estimates
        points(1:points_n + shift, extract.from.summary(summarised_data, 3, rarefaction), col = col[[1]], pch = 19)
    }


    if(observed == TRUE) {
        ## Add the points observed (if existing)
        points(1:points_n + shift, extract.from.summary(summarised_data, 3, rarefaction = FALSE), col = col[[1]], pch = 4)
    }

    ## Save parameters
    return(par())
}

## continuous plotting
plot.continuous <- function(summarised_data, rarefaction, is_bootstrapped, is_distribution, ylim, xlab, ylab, col, time_slicing, observed, add, density, ...) {
    
    ## How many points?
    points_n <- length(unique(summarised_data$subsets))

    ## Set the shift parameter (for add)
    shift = 0
    if(add) {
        ## Is the previous plot the same size?
        prev_axis <- par("xaxp")
        if(prev_axis[2] == points_n) {
            shift = 0
        } else {
            shift = 0.5
        }
    }

    ## Plot the central tendency
    if(add == FALSE) {
        if(time_slicing[1] == FALSE) {
            ## Plot with standard xaxis
            plot((seq(from = 1, to = points_n)-shift), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]], ...)
            #plot((seq(from = 1, to = points_n)-shift), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]]) ; warning("DEBUG: plot")
        } else {
            plot((seq(from = 1, to = points_n)-shift), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]], xaxt = "n", ...)
            #plot((seq(from = 1, to = points_n)-shift), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), type = "l", ylim = ylim, col = col[[1]], xlab = xlab, ylab = ylab[[1]], xaxt = "n") ; warning("DEBUG: plot")
            axis(1, 1:points_n, time_slicing)
        }
    } else {
        lines(seq(from = 1, to = points_n), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), col = col[[1]])
    }

    ## Check if bootstrapped
    if(is_bootstrapped || is_distribution) {
        ## How many quantiles?
        quantiles_n <- (ncol(summarised_data) - ifelse(is_bootstrapped, 4, 3))/2

        ## Set the colours
        if(length(col) < (quantiles_n + 1)) {
            cols_missing <- (quantiles_n + 1) - length(col)
            colfun <- colorRampPalette(c("grey", "lightgrey"))
            col_tmp <- c(col, colfun(cols_missing))
            poly_col <- col_tmp[-1]
            poly_col <- rev(poly_col)
        } else {
            poly_col <- col[-1]
            poly_col <- rev(poly_col)
        }

        ## Add the polygons
        for (cis in 1:quantiles_n) {
            x_vals <- c(1:points_n, points_n:1)
            y_vals <- c(extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3) + cis, rarefaction), rev(extract.from.summary(summarised_data, (ifelse(is_bootstrapped, 4, 3) + quantiles_n*2)-(cis-1), rarefaction)))

            ## Dividing the polygon if NAs
            if(any(is.na(y_vals))) {

                ## Check where the NAs are
                is_nas <- is.na(y_vals[1:points_n])

                ## Selecting the groups of applicable data
                groups <- numeric()
                group_label <- 1
                for(point in 1:length(is_nas)) {
                    if(is.na(y_vals[point])) {
                        group_label <- group_label + 1
                        groups[point] <- NA
                    } else {
                        groups[point] <- group_label
                    }
                }

                ## splitting the data into the groups
                y_vals1 <- split(y_vals[1:points_n], groups)
                y_vals2 <- split(y_vals[(points_n+1):(points_n*2)], rev(groups))
                x_vals1 <- split(x_vals[1:points_n], groups)
                x_vals2 <- split(x_vals[(points_n+1):(points_n*2)], rev(groups))

                ## Merging the groups
                y_vals <- mapply(c, y_vals1, y_vals2, SIMPLIFY = FALSE)
                x_vals <- mapply(c, x_vals1, x_vals2, SIMPLIFY = FALSE)

                ## Plotting the polygons
                mapply(polygon, x_vals, y_vals, MoreArgs = list(col = poly_col[[cis]], border = "NA", density = density))

            } else {
                polygon(x_vals, y_vals, col = poly_col[[cis]], border = "NA", density)
            }

        }

        ## Add the central tendency on top
        lines(seq(from = 1, to = points_n), extract.from.summary(summarised_data, ifelse(is_bootstrapped, 4, 3), rarefaction), lty = 1, col = col[[1]])

        ## Add the observed values on top
        if(observed == TRUE) {
            ## Add the points observed (if existing)
            points(1:points_n, extract.from.summary(summarised_data, 3, rarefaction = FALSE), col = col[[1]], pch = 4)
        }
    }

    ##  Save parameters
    return(par())
}

## Plotting elements
plot.elements <- function(summarised_data, rarefaction, type, ylab, col, element.pch, ...) {

    ## Check if ylab2 exists
    if(length(ylab) == 1) {
        ylab[[2]] <- "Elements"
    }

    ## Add the lines
    if(type == "continuous") {
        ## Continuous (straightforward)
        plot(extract.from.summary(summarised_data, 2, rarefaction), type = "l", lty = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    } else {
        ## Creating the dummy data table
        points_n <- length(unique(summarised_data$subsets))
        dummy_mat <- matrix(extract.from.summary(summarised_data, 2, rarefaction), ncol = points_n)
        colnames(dummy_mat) <- extract.from.summary(summarised_data, 1)
        boxplot(dummy_mat, xaxt = "n", yaxt = "n", xlab = "", ylab = "", boxwex = 0.5/points_n, lty = 2, border = "white", type = "n")
        for(line in 1:points_n) {
            # lines(c(line-0.25, (line+0.25)), rep(summarised_data[line,2], 2), lty = 2, lwd = 1.5)
            points(line, summarised_data[line,2], pch = element.pch, col = col)
        }
    }
    ## lines(extract.from.summary(summarised_data, 2, rarefaction), lty=2)
    axis(4, lty = 2)
    mtext(ylab[[2]], side = 4, line = 2)
}


## Splitting the summarised data table by subsets (list)
split.summary.data <- function(subsets_levels, summarised_data) {
    return(summarised_data[which(summarised_data$subsets == subsets_levels),])
}

## rarefaction plottings
plot.rarefaction <- function(sub_data, ylim, xlab, ylab, col, main, ...) {
    ## Get parameters
    if(ylim[[1]] == "rarefaction") {
        ## ylim?
        ylim <- c(min(sub_data[, -c(1:2)], na.rm = TRUE) - min(sub_data[, -c(1:2)], na.rm = TRUE) * 0.02 , max(sub_data[, -c(1:2)], na.rm = TRUE) + max(sub_data[, -c(1:2)], na.rm = TRUE) * 0.02)
    }
    ## title?
    if(missing(main)) {
        main <- unique(as.character(sub_data$subsets))
    }
    ## how many quantiles?
    quantiles_n <- (ncol(sub_data) - 4)/2

    ## colors?
    if(length(col) < quantiles_n) {
        col <- rep(col[[1]], quantiles_n + 1)
    }

    ## Plot central tendency curve (continuous)
    # if(!missing(main)) {
        plot(rev(sub_data[, 4]), type = "l",  xlab = xlab, ylab = ylab[[1]], col = col[[1]], ylim = ylim, main = main, ...)
    # } else {
        # plot(rev(sub_data[,4]), type = "l",  xlab = xlab, ylab = ylab[[1]], col = col[[1]], ylim = ylim, ...)
    # }


    ## Plot the quantiles curves
    if(quantiles_n != 0) {
        for (cis in 1:quantiles_n) {
            ## lower quantile
            lines(rev(sub_data[, 4 + cis]), lty = (quantiles_n + 2 - cis), col = col[[cis + 1]])
            ## upper quantile
            lines(rev(sub_data[, ncol(sub_data) - (cis - 1)]), lty = (quantiles_n + 2- cis), col = col[[cis + 1]])
        }
    }
    ##  Save parameters
    return(par())
}

## Transposing data for boxploting (taking functions from summary.dispRity)
transpose.box <- function(data, rarefaction, is_bootstrapped) {

    get.rare <- function(data, rare){
        return(data[[rare]])
    }

    if(rarefaction == FALSE) {
        if(is_bootstrapped) {
            ## Select the raw data
            box_data <- lapply(data$disparity, function(X) return(X[[2]]))
        } else {
            box_data <- lapply(data$disparity, function(X) return(X[[1]]))
        }
    } else {
        ## Select the rarefaction data
        rare_rows <- lapply(lapply(data$subsets, lapply, nrow), function(X) which(X[-1] == rarefaction) + 1)
        box_data <- mapply(get.rare, data$disparity, rare_rows, SIMPLIFY = FALSE)
    }

    output <- t(matrix(unlist(box_data), nrow = length(data$subsets), byrow = TRUE))

    colnames(output) <- names(data$subsets)

    return(output)
}

## The following is a modified version of plot.randtest from ade4 v1.4-3
plot.randtest <- function (data_sub, nclass = 10, coeff = 1, ...) {
    
    ## Observed data
    observed <- data_sub$obs
    ## Hist info
    histogram <- data_sub$plot$hist
    ## Plot info
    xlim <- data_sub$plot$xlim
    ylim <- c(0, max(data_sub$plot$hist$count))

    ## Plotting the simulated data
    plot(data_sub$plot$hist, xlim = xlim, col = grey(0.8), ...)

    ## Adding the observed data
    lines(c(observed, observed), c(ylim[2]/2, 0))
    points(observed, ylim[2]/2, pch = 18, cex = 2)

    ## Adding the legend (test results)
    legend("topleft", bty = "n", legend = c("p-value", round(data_sub$pvalue, 5)), cex = 0.7, adj = 0.2)
}

# Plotting model tests results
plot.model.test.support <- function(data, col, ylab, ylim, ...) {

    ## Extracting the weighted aicc
    plot_aic <- data$aic.models[, 3]

    ## Ordering the weighted aicc
    ordered_aic <- plot_aic[order(plot_aic, decreasing = TRUE)]

    ## Plot
    plotcoords <- graphics::barplot(ordered_aic, col = col, ylim = ylim, ylab = ylab, ...)
}

# ~~~~~~~~~~
# sequential.test plots
# ~~~~~~~~~~


## Adding a line
# add.line <- function(xs, ys, lines.args) {
#     if(!is.null(lines.args)) {
#         ## Adding the x,y coordinates
#         lines.args$x <- xs ; lines.args$y <- ys
#         do.call(lines, lines.args)
#     } else {
#         lines(xs, ys)
#     }
# }

## Adding significance tokens
# significance.token <- function(xs, ys, p.value, token.args) {
#     if(p.value < 0.1) {
#         ## Selecting the token
#         if(p.value < 0.1) token <- "."
#         if(p.value < 0.05) token <- "*"
#         if(p.value < 0.01) token <- "**"
#         if(p.value < 0.001) token <- "***"
#         ## Default plotting
#         if(is.null(token.args)) {
#             text(x = sum(xs)/2, y = max(ys)+max(ys)*0.05, token)
#         } else {
#         ## Plotting with arguments
#             token.args$labels <- token
#             token.args$x <- sum(xs)/2
#             if(any(names(token.args) == "float")) {
#                 token.args$y <- max(ys)+max(ys)*token.args$float
#                 token.args$float <- NULL
#             } else {
#                 token.args$y <- max(ys)+max(ys)*0.05
#             }
#             do.call(text, token.args)
#         }
#     }
# }

## Getting the two coordinates of the intercepts (intercept0 and intercept predicted)
# get.intercept.coords <- function(results_out, model_number, is.distribution, significance) {
#     if(is.distribution != TRUE) {
#         ## Get the first y coordinate (first intercept)
#         y1 <- results_out$Intercept[model_number, 1]
#         ## Test if intercept0 is significant
#         if(model_number == 1) {
#             if(results_out$Intercept[model_number, 4] > 0.05) {
#                 y1 <- 0
#             }
#         }

#         ## Get the second y coordinate (second intercept)
#         if(model_number < nrow(results_out$Intercept)) {
#             ## Intercept already estimated
#             y2 <- results_out$Intercept[model_number+1, 1]
#         } else {
#             ## Estimate the intercept
#             if(results_out$Slopes[model_number, 4] < 0.05) {
#                 slope <- results_out$Slopes[model_number, 1]
#             } else {
#                 slope <- 0
#             }
#             y2 <- intercept.estimate(results_out$Intercepts[model_number,1], slope)
#         }
#     } else {
#         ## Get the first y coordinate
#         if(model_number == 1) {
#             ## Get the initial (estimated) intercept if significant
#             if(results_out$Intercept$Initial[4, significance] < 0.05) {
#                 y1 <- unlist(results_out$Intercept$Initial[1, significance])
#             } else {
#                 y1 <- 0
#             }
#         } else {
#             ## Get the predicted intercept
#             y1 <- unlist(results_out$Intercepts$Predicted[model_number-1, significance])
#         }

#         ## Get the second y coordinate (second intercept)
#         if(model_number-1 < nrow(results_out$Intercept$Predicted)) {
#             ## Intercept already estimated
#             y2 <- unlist(results_out$Intercepts$Predicted[model_number, significance])
#         } else {
#             ## Estimate the intercept
#             if(results_out$Slopes$`Pr(>|t|)`[model_number, significance] < 0.05) {
#                 slope <- unlist(results_out$Slopes$Estimate[model_number, significance])
#             } else {
#                 slope <- 0
#             }
#             y2 <- intercept.estimate(unlist(results_out$Intercepts$Predicted[model_number-1, significance]), slope)
#         }        
#     }

#     ## Return the coordinates
#     return(c(y1, y2))
# }

## Plotting the results of sequential tests
# plot.seq.test <- function(results_out, is.distribution, significance, lines.args, token.args) {
#     ## Get the number of models to plot
#     if(is.distribution != TRUE) {
#         n_models <- nrow(results_out$Slopes)
#     } else {
#         n_models <- nrow(results_out$Slopes$Estimate)
#     }

#     ## Loop through each model
#     for(model_number in 1:n_models) {
#         ## Getting x,y coordinates for one model
#         x_coords <- c(model_number, model_number+1)
#         y_coords <- get.intercept.coords(results_out, model_number=model_number, is.distribution, significance)

#         ## Plotting the line
#         add.line(x_coords, y_coords, lines.args)

#         ## Add significance (if necessary)
#         if(is.distribution != TRUE) {
#             p_value <- results_out$Slope[model_number, 4]
#         } else {
#             p_value <- results_out$Slope$`Pr(>|t|)`[model_number, significance]
#             ## get p_value
#         }

#         significance.token(x_coords, y_coords, p_value, token.args)
#     }
# }


#Plot sequential.test shortcut
# if(length(class(data)) == 2) {
#     if(class(data)[[1]] == "dispRity" && class(data)[[2]] == "seq.test") {

#         #lines.args sanitizing
#         if(!is.null(lines.args)) check.class(lines.args, "list")

#         #token.args sanitizing
#         if(!is.null(token.args)) check.class(token.args, "list")

#         #Creating the table results
#         results_out <- summary.seq.test(data, quantiles, cent.tend, recall, digits = 10, results = "coefficients", match_call = list(cent.tend = NULL))

#         #Checking if distribution
#         is_distribution <- ifelse(length(data$models[[1]]) == 1, FALSE, TRUE)

#         #significance sanitizing
#         if(is_distribution == TRUE) {
#             if(class(significance) == "character") {
#                 if(significance != "cent.tend") {stop("significance argument must be either 'cent.tend' or a single 'numeric' value.")}
#                 significance = 1
#             } else {
#                 check.class(significance, "numeric", " must be either 'cent.tend' or a single 'numeric' value.")
#                 check.length(significance, 1, " must be either 'cent.tend' or a single 'numeric' value.")
#                 if(is.na(match(significance, seq(from = 1, to = length(quantiles)*2)))) {
#                     stop("significance argument must be the number of the quantile (e.g. 1 for the first quantile).")
#                 } else {
#                     significance = significance + 1
#                 }
#             }
#         }


#         #Plotting the results
#         if(add != TRUE) {
#             #subsamples
#             subsamples <- unique(unlist(strsplit(names(data$models), split = " - ")))
#             #Get the all the intercepts estimate
#             if(is_distribution == TRUE) {
#                 all_intercepts <- unlist(c(results_out$Intercepts$Initial[1,significance], results_out$Intercepts$Predicted[,significance], intercept.estimate(unlist(results_out$Intercepts$Predicted[(length(subsamples)-2),significance]), unlist(results_out$Slopes$Estimate[(length(subsamples)-1),significance]))))
#             } else {
#                 all_intercepts <- c(results_out$Intercepts[,1], intercept.estimate(results_out$Intercepts[(length(subsamples)-1),1], results_out$Slopes[(length(subsamples)-1),1]))
#             }
            
#             if(missing(xlab)) {
#                 xlab <- "subsamples"
#             }
#             if(missing(ylab)) {
#                 ylab <- "Estimated disparity"
#             }

#             #Empty plot
#             subsamples_length <- length(subsamples)
#             plot(seq(from = 1, to = subsamples_length), all_intercepts, col = "white", xlab = xlab, ylab = ylab, xaxt = "n", ...)
#             #plot(seq(from = 1, to = subsamples_length), all_intercepts, col = "white", xlab = xlab, ylab = ylab, xaxt = "n") ; warning("DEBUG in plot.dispRity")
#             axis(1, at = 1:subsamples_length, labels = subsamples)
#         }

#         plot.seq.test(results_out, is_distribution, significance, lines.args, token.args)

#     }

