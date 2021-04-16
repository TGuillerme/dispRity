#context("reduce.space")

# stop("DEBUG")
# library(testthat)
# library(dispRity)
# source("../reduce.space.R")
# source("../reduce.space_fun.R")



## Test
test_that("reduce.space works", {

    ## Sanitizing
    expect_error(reduce.space())

    set.seed(42)
    space <- dispRity::space.maker(300, 2, distribution = rnorm)

    ## Sanitizing
    error <- capture_error(reduce.space(c(1,2), type = "random", remove = 0.3, parameters = list("bw" = bw.nrd0), tuning = list("max" = 100)))
    expect_equal(error[[1]], "space must be of class matrix or data.frame.")
    error <- capture_error(reduce.space(space, type = "RANDOM", remove = 0.3, parameters = list("bw" = bw.nrd0), tuning = list("max" = 100)))
    expect_equal(error[[1]], "Reduction type must be one of the following: random, size, position, evenness, density.")
    error <- capture_error(reduce.space(space, type = "random", remove = -20, parameters = list("bw" = bw.nrd0), tuning = list("max" = 100)))
    expect_equal(error[[1]], "remove must be a probability or a percentage.")
    error <- capture_error(reduce.space(space, type = "random", remove = 0.3, parameters = bw.nrd0, tuning = list("max" = 100)))
    expect_equal(error[[1]], "parameters must be a named list of parameters.")
    error <- capture_error(reduce.space(space, type = "random", remove = 0.3, parameters = list(bw.nrd0), tuning = list("max" = 100)))
    expect_equal(error[[1]], "list(bw.nrd0) must be a named list of parameters.")
    error <- capture_error(reduce.space(space, type = "random", remove = 0.3, parameters = list("bw" = bw.nrd0), tuning = c("max" = 100)))
    expect_equal(error[[1]], "tuning must be a named list of tuning parameters.")
    error <- capture_error(reduce.space(space, type = "random", remove = 0.3, parameters = list("bw" = bw.nrd0), tuning = list(100)))
    expect_equal(error[[1]], "list(100) must be a named list of tuning parameters.")

    ## Random removal, super easy
    error <- capture_error(test <- reduce.space(space, type = "random", remove = 101))
    expect_equal(error[[1]], "remove must be a probability or a percentage.")
    test <- reduce.space(space, type = "random", remove = 30)
    expect_is(test, "logical")
    expect_equal(length(test), 300)
    expect_equal(length(which(test)), 90)

    ## Limit removal
    set.seed(1)
    iter <- capture_output(test1 <- reduce.space(space, type = "size", remove = 0.5, verbose = TRUE))
    expect_is(test1, "logical")
    expect_equal(length(test1), 300)
    expect_equal(length(which(test1)), 149)
    expect_equal(iter, "Run parameter optimisation:............Done.")

    set.seed(1)
    test2 <- reduce.space(space, type = "size", parameters = list("radius" = 1.206866))
    expect_is(test2, "logical")
    expect_equal(length(test2), 300)
    expect_equal(length(which(test2)), 149)

    expect_equal(test1, test2)

    set.seed(1)
    test3 <- reduce.space(space, type = "size", remove = 0.5, return.optim = TRUE)
    expect_equal(round(test3[[2]], 6), round(1.206866, 6))
    expect_equal(length(which(test3[[1]])), 149)
    expect_equal(test1, test3$remove)

    ## Displacement removal
    set.seed(1)
    iter <- capture_output(test1 <- reduce.space(space, type = "position", remove = 0.5, verbose = TRUE, tuning = list("tol" = 0.1)))
    expect_is(test1, "logical")
    expect_equal(length(test1), 300)
    expect_equal(length(which(test1)), 152)
    expect_equal(iter, "Run parameter optimisation:........Done.")

    set.seed(1)
    test2 <- reduce.space(space, type = "position", parameters = list("radius" = 4.390509))
    expect_is(test2, "logical")
    expect_equal(length(test2), 300)
    expect_equal(length(which(test2)), 152)

    expect_equal(test1, test2)

    set.seed(1)
    test3 <- reduce.space(space, type = "position", remove = 0.5, return.optim = TRUE, tuning = list("tol" = 0))
    expect_equal(round(test3[[2]], 6), 4.390509)
    expect_equal(length(which(test3[[1]])), 152)


    ## Density removal
    set.seed(1)
    iter <- capture_output(test1 <- reduce.space(space, type = "density", remove = 0.5, verbose = TRUE))
    expect_is(test1, "logical")
    expect_equal(length(test1), 300)
    expect_equal(length(which(test1)), 151)
    expect_equal(iter, "Run parameter optimisation:............Done.")

    set.seed(1)
    test2 <- reduce.space(space, type = "density", parameters = list("diameter" = 0.1015625))
    expect_is(test2, "logical")
    expect_equal(length(test2), 300)
    expect_equal(length(which(test2)), 151)

    expect_equal(test1, test2)

    set.seed(1)
    test3 <- reduce.space(space, type = "density", remove = 0.5, return.optim = TRUE)
    expect_equal(test3[[2]], 0.1015625)
    expect_equal(length(which(test3[[1]])), 151)

    expect_equal(test1, test3$remove)


    ## Evenness removal
    set.seed(1)
    test <- reduce.space(space, type = "evenness", remove = 0.7)
    expect_is(test, "logical")
    expect_equal(length(test), 300)
    expect_equal(length(which(test)), 210)

    ## Evenness with exageration
    set.seed(1)
    test2 <- reduce.space(space, type = "evenness", remove = 0.5, parameters = list(power = 2))
    expect_equal(length(test2), 300)
    expect_equal(length(which(test2)), 150)

    ## Evenness test visual
    visualise.evenness <- function(space, remove, ...) {

        selected <- reduce.space(space, type = "evenness", remove = remove, ...)

        nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(2.5,1.5), c(1.5,2.5), TRUE)

        ## Plotting the points
        par(mar = c(3,3,1,1))
        plot(space[!selected,], pch = 19, col = "blue")
        points(space[selected,], pch = 19, col = "orange")

        ## Plotting the distributions
        all_range <- range(c(space))
        band_width <- bw.nrd0(c(space))
        bin_breaks <- seq(from = min(c(space)), to = max(c(space) + band_width), by = band_width) 
        x_hist_all <- hist(space[,1], breaks = bin_breaks, plot = FALSE)
        y_hist_all <- hist(space[,2], breaks = bin_breaks, plot = FALSE)
        x_hist_sel <- hist(space[selected,1], breaks = bin_breaks, plot = FALSE)
        x_hist_rem <- hist(space[!selected,1], breaks = bin_breaks, plot = FALSE)
        y_hist_sel <- hist(space[selected,2], breaks = bin_breaks, plot = FALSE)
        y_hist_rem <- hist(space[!selected,2], breaks = bin_breaks, plot = FALSE)

        top <- max(c(x_hist_all$counts, y_hist_all$counts))

        par(mar = c(0,3,1,1))
        barplot(x_hist_all$counts, axes = FALSE, ylim = c(0, top), space = 0, col = "grey")
        barplot(x_hist_sel$counts, axes = FALSE, ylim = c(0, top), space = 0, col = "orange", add = TRUE)
        barplot(x_hist_rem$counts, axes = FALSE, ylim = c(0, top), space = 0, col = "blue", add = TRUE, density = 75)

        par(mar = c(0,3,1,1))
        barplot(y_hist_all$counts, axes = FALSE, xlim = c(0, top), space = 0, col = "grey", horiz = TRUE)
        barplot(y_hist_sel$counts, axes = FALSE, xlim = c(0, top), space = 0, col = "orange", add = TRUE, horiz = TRUE)
        barplot(y_hist_rem$counts, axes = FALSE, xlim = c(0, top), space = 0, col = "blue", add = TRUE, horiz = TRUE, density = 75)
    }

    ## Both distributions are around 50% of the total distribution (in grey)
    visualise.evenness(space, remove = 0.5)
    ## We've flattened the curve for the orange distribution!
    visualise.evenness(space, remove = 0.8)
    ## same for the blue one
    visualise.evenness(space, remove = 0.2)
    ## With exageration
    visualise.evenness(space, remove = 0.5, parameters = list(power = 2))

})

