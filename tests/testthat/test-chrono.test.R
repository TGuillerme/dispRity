set.seed(123)
tree <- rtree(n = 100)
tree <- makeNodeLabel(tree)
tree <- set.root.time(tree)
mat <- matrix(rnorm(995), 199, 5)
rownames(mat) <- c(tree$tip.label, tree$node.label)
data <- make.dispRity(data = mat, tree = tree)
data <- chrono.subsets(data, method = "c", model = "equal.split", time = 10, inc.nodes = TRUE)
## Warning is for the last time slice that's 0
expect_warning(data <- dispRity(data, metric = mean))

test_that("sanitizing works", {
	data(disparity)
	wrong_data <- dispRity(custom.subsets(make.dispRity(data = mat, tree = tree), group = tree), metric = mean)

	error <- capture_error(chrono.test("disparity", method = "average", changepoint = 66))
	expect_equal(error[[1]], "disparity must be a dispRity object with a tree, time series and disparity data.")
	error <- capture_error(chrono.test(wrong_data, method = "average", changepoint = 66))
	expect_equal(error[[1]], "chrono.test is not implemented yes for customised subsets")
	error <- capture_error(chrono.test(remove.tree(disparity), method = "average", changepoint = 66))
	expect_equal(error[[1]], "remove.tree(disparity) must be a dispRity object with a tree, time series and disparity data.")
	wrong_data <- disparity
	wrong_data$disparity <- NULL
	error <- capture_error(chrono.test(wrong_data, method = "average", changepoint = 66))
	expect_equal(error[[1]], "wrong_data must be a dispRity object with a tree, time series and disparity data.")

	error <- capture_error(chrono.test(disparity, method = "average", changepoint = 140))
	expect_equal(error[[1]], "changepoint falls out of the time range of the data tree (0 - 139.0743).")
	error <- capture_error(chrono.test(disparity, method = "average", changepoint = c(66, 140)))
	expect_equal(error[[1]], "changepoints falls out of the time range of the data tree (0 - 139.0743).")

	error <- capture_error(chrono.test(disparity, method = "average", changepoint = 66, time.window = c(0, 140)))
	expect_equal(error[[1]], "time.windows falls out of the time range of the data tree (0 - 139.0743).")

	error <- capture_error(chrono.test(disparity, method = "averagesss", changepoint = 66, time.window = 3))
	expect_equal(error[[1]], "method argument must be one of the following: itsa, citsa, area, average.")
	error <- capture_error(chrono.test(disparity, method = t.test, changepoint = 66, time.window = 3))
	expect_equal(error[[1]], "user function for method not implemented yet.")
})

test_that("make.deltatronic works", {
	data(disparity)
	########changepoint testing#################
	changepoint  <- 66
	changepoint  <- set.changepoint(changepoint)
	expect_is(changepoint, "list")
	expect_equal(as.numeric(names(changepoint)), changepoint[[1]])
	changepoint <- "detect"
	changepoint <- set.changepoint(changepoint)
	changepoint <- c(120, 66)
	changepoint <- set.changepoint(changepoint)
	expect_is(changepoint, "list")
	expect_equal(length(changepoint), 2)

	###### test make.deltatronic.list#######
	changepoint <- 66
	delta_df <- make.deltatronic.list(changepoint, disparity)
	expect_is(delta_df, "list")
	expect_equal(nrow(delta_df$time), length(disparity$subsets))
	expect_equal(nrow(delta_df$disparity), length(disparity$subsets))
    expected_impact <- ifelse(delta_df$time <= changepoint, 1, 0)
    expect_equal(as.vector(delta_df$impact), as.vector(expected_impact))
    expect_true(any(delta_df$impact == 0))
    expect_true(any(delta_df$impact == 1))
    first_impact_index <- min(which(delta_df$time <= changepoint))
    expect_equal(as.numeric(delta_df$impact[first_impact_index, ]), 1)
    expect_equal(as.numeric(delta_df$impact[first_impact_index - 1, ]), 0)
	expect_true(all(diff(delta_df$time_elapsed)>0)) ## check time elapsed is increasing
	expect_true(all(diff(delta_df$time)<0)) ## check raw time is decreasing
	expect_true(all(diff(delta_df$time_post_cp)>=0))

	## test set.time.window
	#### n datapoints
	time.window  <- 3
	datapoints_window <- set.time.window(delta_df, time.window)
	expect_true(all(unlist(lapply(datapoints_window, nrow)) == 6)) ## 3 datapoints either sie
	expect_true(sum(datapoints_window$impact == 0) == sum(datapoints_window$impact == 1)) ## equal number of 0 and 1

	time.window <- c(70, 50)
	error <- capture_error(set.time.window(delta_df, time.window))
	expect_equal(error[[1]], "time.window window is too small. Needs at least 2 datapoints either side of the impact to run the function...\n")
	time.window <- c(80, 50)
	vector_window <- set.time.window(delta_df, time.window)
	expect_true(all(unlist(lapply(vector_window, nrow)) == 4))## 2 datapoints either side
	expect_true(min(vector_window$time) == 50)
	expect_true(max(vector_window$time) == 80)


	time.window <- 0.3
	prop_window <- set.time.window(delta_df, time.window)
	expect_true(min(prop_window$time) == 40)
	expect_true(max(prop_window$time) == 80)

	### test make.deltatronic #### 
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL) ## test without time.window
	expect_equal(names(delta_df), "66")
	expect_true(all(diff(delta_df[[1]]$time_elapsed)>0)) ## check time elapsed is increasing
	expect_true(all(diff(delta_df[[1]]$time)<0)) ## check raw time is decreasing
	expect_true(all(diff(delta_df[[1]]$time_post_cp)>=0))
	expect_equal(nrow(delta_df[[1]]$time), length(disparity$subsets))
	expect_equal(nrow(delta_df[[1]]$disparity), length(disparity$subsets))
    expected_impact <- ifelse(delta_df[[1]]$time <= changepoint, 1, 0)
    expect_equal(as.vector(delta_df[[1]]$impact), as.vector(expected_impact))
    expect_true(any(delta_df[[1]]$impact == 0))
    expect_true(any(delta_df[[1]]$impact == 1))
    first_impact_index <- min(which(delta_df[[1]]$time <= changepoint))
    expect_equal(as.numeric(delta_df[[1]]$impact[first_impact_index, ]), 1)
    expect_equal(as.numeric(delta_df[[1]]$impact[first_impact_index - 1, ]), 0)

	delta_df <- make.deltatronic(disparity, 66, time.window = 3) ## test without time.window
	expect_true(all(unlist(lapply(delta_df[[1]], nrow)) == 6)) ## 3 datapoints either sie
	expect_true(sum(delta_df[[1]]$impact == 0) == sum(delta_df[[1]]$impact == 1)) ## equal number of 0 and 1

	expect_is(delta_df, "list")
	expect_is(delta_df[[1]], "list")
	expect_true(all(unlist(lapply(delta_df, lapply, is.matrix))))
	expect_true(all(names(delta_df[[1]]) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp")))	
	# delta_df <- make.deltatronic(disparity, 66, time.window = NULL) ## no error



	## test it works with multicolumn disparity
	set.seed(123)
	tree <- rtree(n = 100)
	tree <- makeNodeLabel(tree)
	tree <- set.root.time(tree)
	changepoint <- tree$root.time / 2
	mat <- matrix(rnorm(995), 199, 5)
	rownames(mat) <- c(tree$tip.label, tree$node.label)
	data <- make.dispRity(data = mat, tree = tree)
	data <- chrono.subsets(data, method = "c", model = "equal.split", time = c(7,6,5,4,3,2,1), inc.nodes = TRUE)
	## Warning is for the last time slice that's 0
	data <- dispRity(data, metric = variances)
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL)
	expect_true(all(delta_df[[1]]$disparity == do.call(rbind, get.disparity(data))))

})


test_that("average.method works", {
	## TODO caleb
	data(disparity)
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
	average <- lapply(delta_df, average.method)#
	expect_is(average[[1]], "htest")
	expect_equal(average[[1]]$method, "Welch Two Sample t-test")
	average <- lapply(delta_df, average.method, alternative = "less")#
	expect_equal(average[[1]]$alternative, "less")
	average <- lapply(delta_df, average.method, aov)
	expect_is(average[[1]], "aov")
	data(disparity)
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
	average <- lapply(delta_df, average.method, wilcox.test)#
	expect_equal(average[[1]]$method, "Wilcoxon rank sum exact test")
	average <- lapply(delta_df, average.method, wilcox.test, alternative = "less")#
	expect_equal(average[[1]]$alternative, "less")


	set.seed(123)
	tree <- rtree(n = 100)
	tree <- makeNodeLabel(tree)
	tree <- set.root.time(tree)
	changepoint <- tree$root.time / 2
	mat <- matrix(rnorm(995), 199, 5)
	rownames(mat) <- c(tree$tip.label, tree$node.label)
	data <- make.dispRity(data = mat, tree = tree)
	data <- chrono.subsets(data, method = "c", model = "equal.split", time = c(7,6,5,4,3,2,1), inc.nodes = TRUE)
	## Warning is for the last time slice that's 0
	data <- dispRity(data, metric = variances)
	dims <- max(data$call$dimensions)
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL)
	average <- lapply(delta_df, average.method, wilcox.test, dimension.level = dims)#
	expect_equal(dims, length(average[[1]]))



}
)

test_that("itsa.method works", {
	data(disparity)
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
	method <- lapply(delta_df, itsa.method, dimension.level = 1)#
	expect_is(method[[1]], "list")
	expect_true(all(names(method[[1]]) %in% c("data", "model")))
	expect_true(all(names(method[[1]]$data) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp", "counter_mean_ci", "counter_lower_ci", "counter_upper_ci")))
	expect_is(method[[1]]$model, "lm")

	set.seed(123)
	tree <- rtree(n = 100)
	tree <- makeNodeLabel(tree)
	tree <- set.root.time(tree)
	changepoint <- tree$root.time / 2
	mat <- matrix(rnorm(995), 199, 5)
	rownames(mat) <- c(tree$tip.label, tree$node.label)
	data <- make.dispRity(data = mat, tree = tree)
	data <- chrono.subsets(data, method = "c", model = "equal.split", time = c(7,6,5,4,3,2,1), inc.nodes = TRUE)
	## Warning is for the last time slice that's 0
	data <- dispRity(data, metric = variances)
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL)
	dims <- max(data$call$dimensions)
	method <- lapply(delta_df, itsa.method, dimension.level = dims)#
}
)

test_that("calculate.angular.effect works", {
	## test for no change
	set_state <- set.seed(123)
	time_elapsed <- seq(0, 60, by = 5)
	baseline_slope <- 0.002
	intercept      <- 1.82
	noise <- rnorm(length(time_elapsed), mean = 0, sd = 0.005)
	disparity <- intercept + (baseline_slope * time_elapsed) + noise
	delta_df <- list(
	time_elapsed = as.matrix(time_elapsed),
	disparity    = as.matrix(disparity),
	impact       = as.matrix(ifelse(time_elapsed >= 30, 1, 0)),
	time_post_cp = as.matrix(ifelse(time_elapsed > 30, time_elapsed - 30, 0))
	)

	itsa <- itsa.method(delta_df, dimension.level = 1)

	calculate.slope.effect(itsa)

	set_state <- set.seed(123)
	time_elapsed <- seq(0, 60, by = 5)

	intercept      <- 1.82
	baseline_slope <- 0.002
	slope_change   <- 0.0005  

	time_post_cp_vec <- ifelse(time_elapsed > 30, time_elapsed - 30, 0)

	noise <- rnorm(length(time_elapsed), mean = 0, sd = 0.005)
	disparity <- intercept + (baseline_slope * time_elapsed) + (slope_change * time_post_cp_vec) + noise

	delta_df <- list(
	time_elapsed = as.matrix(time_elapsed),
	disparity    = as.matrix(disparity),
	impact       = as.matrix(ifelse(time_elapsed >= 30, 1, 0)),
	time_post_cp = as.matrix(time_post_cp_vec)
	)

	itsa <- itsa.method(delta_df, dimension.level = 1)

	plot(disparity ~ time_elapsed, data = delta_df, type = "l")

	print(calculate.slope.effect(itsa))

	data(disparity)
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
	method <- lapply(delta_df, itsa.method, dimension.level = 1)#
	expect_is(method[[1]], "list")
	expect_true(all(names(method[[1]]) %in% c("data", "model")))
	expect_true(all(names(method[[1]]$data) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp", "counter_mean_ci", "counter_lower_ci", "counter_upper_ci")))
	expect_is(method[[1]]$model, "lm")

}
)

test_that("paint.branches works", {
	tree  <- rtree(n=50)
	expect_is(tree, "phylo")
	tree <- set.root.time(tree)
	expect_is(tree$root.time, "numeric")
	cp <- tree$root.time/2	
	painted <- paint.branches(tree, cp)
	expect_is(painted, "simmap")
	expect_true(all(c("pre_impact", "pre_impact") %in% names(unlist(painted$maps))))
	# error <- capture_error(paint.branches(tree, changepoint = 50))
}
)

test_that("make.control works", {
	data(disparity)
	changepoint <- 66
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
	changepoint <- set.changepoint(changepoint)
	control <- lapply(changepoint, make.control, data = disparity, paint = TRUE, nsim = 10)
	expect_is(control, "list")
	expect_equal(names(control), "66")
	expect_equal(length(control[[1]]$disparity$subsets), length(disparity$subsets))
	expect_equal(length(get.disparity(control[[1]]$disparity)[[1]]), length(get.disparity(disparity)[[1]]) * 10)
	error <- capture_error(lapply(changepoint, make.control, data = disparity, paint = FALSE, nsim = 10))
	expect_equal(error[[1]], "`slice.model` argument needs to be inputted if paint = FALSE...\n")
	error <- capture_error(lapply(changepoint, make.control, data = disparity, paint = FALSE, slice.model = 5, nsim = 10))
	expect_equal(error[[1]], "slice.model argument must be one of the following: acctran, deltran, random, proximity, equal.split, gradual.split.")
	expect_equal(names(control[[1]]), c("sim_parameters", "disparity"))
	expect_equal(ncol(control[[1]]$sim_parameters),ncol(get.matrix(disparity)))
	expect_equal(nrow(get.matrix(control[[1]]$disparity)), nrow(get.matrix(disparity)))


	set.seed(123)
	tree <- rtree(n = 100)
	tree <- makeNodeLabel(tree)
	tree <- set.root.time(tree)
	changepoint <- tree$root.time / 2
	mat <- matrix(rnorm(995), 199, 5)
	rownames(mat) <- c(tree$tip.label, tree$node.label)
	data <- make.dispRity(data = mat, tree = tree)
	data <- chrono.subsets(data, method = "c", model = "equal.split", time = c(7,6,5,4,3,2,1), inc.nodes = TRUE)
	## Warning is for the last time slice that's 0
	data <- dispRity(data, metric = variances)
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL)
	dims <- max(data$call$dimensions)
	changepoint <- set.changepoint(changepoint)
    control <- lapply(changepoint, make.control, data = data, nsim = 5)
	
	## multi matrix
	set.seed(123)
	tree <- rtree(n = 100)
	tree <- makeNodeLabel(tree)
	tree <- set.root.time(tree)
	changepoint <- tree$root.time / 2
	mat <- replicate(10, matrix(rnorm(995), 199, 5), simplify = FALSE)
	mat <- lapply(mat, function(x) {
	rownames(x)  <- c(tree$tip.label, tree$node.label)#
	return(x)
	})
	data <- make.dispRity(data = mat, tree = tree)
	data <- chrono.subsets(data, method = "c", model = "equal.split", time = c(7,6,5,4,3,2,1), inc.nodes = TRUE)
	## Warning is for the last time slice that's 0
	disp <- dispRity(data, metric = c(sum, variances))


	delta_df <- make.deltatronic(disp, changepoint, time.window = NULL)
	dims <- max(data$call$dimensions)
	changepoint <- set.changepoint(changepoint)
    control <- lapply(changepoint, make.control, data = data, nsim = 5)
	



}
)


## eg multi.ace sample = >1 output
test_that("multi matrix disparity works", {
	set.seed(123)
	tree <- rtree(n = 100)
	tree <- makeNodeLabel(tree)
	tree <- set.root.time(tree)
	changepoint <- tree$root.time / 2
	mat_1 <- matrix(rnorm(995), 199, 5)
	mat_2 <- matrix(rnorm(995), 199, 5)
	rownames(mat_2)	 <- rownames(mat_1) <- c(tree$tip.label, tree$node.label)

	multi_data <- make.dispRity(list(mat_1, mat_2), tree)
	multi_data <- chrono.subsets(multi_data, method = "c", model = "equal.split", time = c(7,6,5,4,3,2,1), inc.nodes = TRUE)
	sum.var <- function(mat){
		sum(variances(mat))
	}
	multi_data_disp <- dispRity(multi_data, metric = sum.var)
	
	var.fun <- function(mat){
		variances(mat)
	}
	multi_multi_dimensional <- dispRity(multi_data, metric = var.fun)



})