context("merge.subsamples")


## merge.two.subsamples
test_that("merge.two.subsamples", {
    # ## Merging two subsamples
    # merge.two.subsamples <- function(subs1, subs2, data) {
    #     ## Get the list of new sub-samples
    #     new_subsample <- list("elements" = matrix(unique(c(data$subsamples[[subs1]], data$subsamples[[subs2]], ncol = 1))))
    #     ## Replace the second subsample by the new one
    #     data$subsamples[[subs2]] <- new_subsample
    #     ## Rename it
    #     names(data$subsamples)[subs2] <- paste(names(data$subsamples)[subs1], names(data$subsamples)[subs2], sep = "-") 
    #     ## Remove the former
    #     data$subsamples[[subs1]] <- NULL
    #     return(data)
    # }
})