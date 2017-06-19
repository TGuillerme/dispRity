
# dyn.load("metrics.so")

# mat <- matrix(1, 4, 5)

# test <- function(matrix, level, metric) {

#     metric <- ifelse(metric == "mean", 0, 1)

#     ## Run the fun
#     return(.Call("matrix_handler", matrix = matrix, level_in = as.integer(level), metric_in = metric))
# }



# set.seed(1)
# mat <- matrix(rnorm(100), 25, 25)
# mean(mat)
# test(mat, 1, "mean")