# Future functions
 * `how.many.bootstraps`: a `optim.replicate` wrapper for choosing the number of bootstraps
 * `which.metric`: a `test.metric` wrapper for choosing between different metrics.


# `dispRitreats`

 * new internal function `dispRitreats` that transforms treats data into `data` and `tree`.
    * If trees are different it uses scaling trees
    * If matrix are different it uses lapply




Handeling the data from treats straight to `dispRity`.

`custom.subsets` `chrono.subsets` and `dispRity` should be able to handle data that is `dispRity` `treats` in `data` (handles the `tree` automatically). If the matrices are the same, run on multiple trees. If the matrices are different do following lapply and relativise the disparity metric output (write a warning message for that):

```
measure.disparity <- function(one_simulation) {
    ## transform the data
    sim <- dispRitreats(one_simulation, scale.tree = FALSE)
    ## Calculate disparity
    disparity <- dispRity(
             chrono.subsets(data = sim$data, tree = sim$tree,
                            method = "continuous", model = "acctran",
                            time = seq(from = 30, to = 0, by = -5)),
                            metric = c(sum, variances))
    ## Scale the disparity
    disparity <- unlist(get.disparity(disparity))
    return(disparity/max(disparity))
}

## Measuring disparities
random_extinction_disparity <- do.call(rbind, lapply(sim_rand_extinction, measure.disparity))
```