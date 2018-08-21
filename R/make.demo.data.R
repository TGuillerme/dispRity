# ## Developer function for making the disparity data
# make.demo.data_disparity <- function() {
#     file.remove("../data/disparity.rda")
#     set.seed(1)
#     ## Getting the data ready
#     data(BeckLee_tree) ; data(BeckLee_mat99) ; data(BeckLee_ages)

#     ## Creating the time subsets
#     time_subsets <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = c(90, 80, 70, 60, 50, 40, 30), FADLAD = BeckLee_ages)

#     ## Bootstrapping the subsets
#     bs_subsets <- boot.matrix(chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = c(90, 80, 70, 60, 50, 40, 30), FADLAD = BeckLee_ages), bootstraps = 100, rarefaction = c(20,15,10,5))

#     ## Calculating disparity
#     disparity <- dispRity(bs_subsets, metric = c(median, centroids))

#     ## save the data
#     save(disparity, file = "../data/disparity.rda")
# }

# ## Developer function for making the disparity data
# make.demo.data_BeckLee_disparity <- function() {
#     file.remove("../data/BeckLee_disparity.rda")
#     set.seed(1)
#     ## Getting the data ready
#     data(BeckLee_tree) ; data(BeckLee_mat99) ; data(BeckLee_ages)

#     ## Creating the time subsets
#     time_subsets <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "proximity", time = seq(from = 0, to = 120, by = 1), FADLAD = BeckLee_ages)

#     ## Bootstrapping the subsets
#     bs_subsets <- boot.matrix(time_subsets, bootstraps = 100)

#     ## Calculating disparity
#     BeckLee_disparity <- dispRity(bs_subsets, metric = c(sum, variances))

#     ## save the data
#     save(BeckLee_disparity, file = "../data/BeckLee_disparity.rda")
# }