make.lapply.loop.resample <- function(one_subset, one_sample_pop, replicates) {
    return(list("elements" = one_subset, replicate(replicates, sample(one_sample_pop, length(one_subset), replace = TRUE))))
}

make.lapply.loop.nosample <- function(one_subset, one_sample_pop, replicates) {
    ## Check if the subset is not the whole population
    if(all(c(one_subset) %in% one_sample_pop)) {
        ## If it is, use make.lapply.loop.resample
        return(make.lapply.loop.resample(one_subset, one_sample_pop, replicates))
    } else {
        return(list("elements" = one_subset, replicate(replicates, sample((one_sample_pop)[-one_subset], length(one_subset), replace = TRUE))))
    }
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


## Get the comparisons to match the subset list
get.sample.pop <- function(one_rand, data) {
    if(length(one_rand) == 0) {
        return(1:nrow(data$matrix[[1]]))
    } else {
        return(sort(unique(unlist(lapply(data$subsets[unlist(one_rand)], function(x) return(x$elements))))))
    }
}

get.sample.pop.name <- function(one_rand, data) {
    if(length(one_rand) == 0) {
        return("the whole space")
    } else {

        orthograph <- paste0("subset", ifelse(length(one_rand) == 1, " ", "s "))

        if(length(one_rand) <= 2) {
            return(paste0(orthograph, paste(one_rand, collapse = " and ")))
        } else {
            return(paste0(orthograph, paste(c(paste(one_rand[-length(one_rand)], collapse = ", "), one_rand[length(one_rand)]), collapse = " and ")))
        }
    }
}