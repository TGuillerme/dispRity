## Developer function for making the disparity data
make.demo.data <- function() {
    file.remove("../data/disparity.rda")
    set.seed(1)
    ## Getting the data ready
    data(BeckLee_tree) ; data(BeckLee_mat99) ; data(BeckLee_ages)

    ## Creating the time subsamples
    time_subsamples <- time.subsamples(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = c(90, 80, 70, 60, 50, 40, 30), FADLAD = BeckLee_ages)

    ## Bootstrapping the subsamples
    bs_subsamples <- boot.matrix(time.subsamples(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = c(90, 80, 70, 60, 50, 40, 30), FADLAD = BeckLee_ages), bootstraps = 100, rarefaction = c(20,15,10,5))

    ## Calculating disparity
    disparity <- dispRity(bs_subsamples, metric = c(median, centroids))

    ## save the data
    save(disparity, file = "../data/disparity.rda")
}