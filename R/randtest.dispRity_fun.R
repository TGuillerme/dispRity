make.lapply.loop.resample <- function(one_subset, replicates, pop_size) {
    return(list("elements" = one_subset, replicate(replicates, sample(1:pop_size, length(one_subset), replace = TRUE))))
}

make.lapply.loop.nosample <- function(one_subset, replicates, pop_size) {
    return(list("elements" = one_subset, replicate(replicates, sample((1:pop_size)[-one_subset], length(one_subset), replace = TRUE))))
}

one.randtest <- function(results, replicates, resample, alternative, get.p.value, match_call) {
    observed <- c(results$elements)
    simulated <- c(results[[2]])

    ## Getting the test results
    test_results <- c("Mean Normal residuals" = mean((observed - mean(simulated)) / stats::sd(simulated)), "Random mean" = mean(simulated), "Random variance" = stats::var(simulated))

    ## Calculating the p-value
    p_value <- get.p.value(simulated, observed, replicates)

    ## Making the results into a randtest object
    res <- list()

    ## Adding the default arguments
    res$rep <- replicates
    res$observed <- observed
    res$random <- simulated
    res$call <- match_call

    ## Adding the data
    res$sim <- simulated
    res$obs <- observed

    ## Adding the plot options (modified from ade4::as.randtest)
    r0 <- c(simulated, observed)
    l0 <- max(simulated, observed) - min(simulated, observed)
    w0 <- l0/(log(length(simulated), base = 2) + 1)
    xlim0 <- range(r0) + c(-w0, w0)
    h0 <- graphics::hist(simulated, plot = FALSE, nclass = 10)
    res$plot <- list(hist = h0, xlim = xlim0)

    ## Adding the test.parameter arguments
    res$alter <- alternative
    res$pvalue <- p_value
    res$expvar <- test_results

    class(res) <- "randtest"
    return(res)
}