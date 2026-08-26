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
	n.matrix <- 1
    if (length(disparity$matrix) > 1){
        n.matrix  <- length(disparity$matrix)
    }
    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }
	delta_df <- make.deltatronic.list(changepoint, disparity, dimension.level, n.matrix)
	expect_is(delta_df[[1]], "list")
	expect_equal(nrow(delta_df[[1]]$time), length(disparity$subsets))
	expect_equal(nrow(delta_df[[1]]$disparity), length(disparity$subsets))
    expected_impact <- ifelse(delta_df[[1]]$time <= changepoint, 1, 0)
    expect_equal(as.vector(delta_df[[1]]$impact), as.vector(expected_impact))
    expect_true(any(delta_df[[1]]$impact == 0))
    expect_true(any(delta_df[[1]]$impact == 1))
    first_impact_index <- min(which(delta_df[[1]]$time <= changepoint))
    expect_equal(as.numeric(delta_df[[1]]$impact[first_impact_index, ]), 1)
    expect_equal(as.numeric(delta_df[[1]]$impact[first_impact_index - 1, ]), 0)
	expect_true(all(diff(delta_df[[1]]$time_elapsed)>0)) ## check time elapsed is increasing
	expect_true(all(diff(delta_df[[1]]$time)<0)) ## check raw time is decreasing
	expect_true(all(diff(delta_df[[1]]$time_post_cp)>=0))

	## test set.time.window
	#### n datapoints
	time.window  <- 3
	datapoints_window <- set.time.window(delta_df[[1]], time.window)
	expect_true(all(unlist(lapply(datapoints_window, nrow)) == 6)) ## 3 datapoints either sie
	expect_true(sum(datapoints_window$impact == 0) == sum(datapoints_window$impact == 1)) ## equal number of 0 and 1

	time.window <- c(70, 50)
	error <- capture_error(set.time.window(delta_df[[1]], time.window))
	expect_equal(error[[1]], "time.window window is too small. Needs at least 2 datapoints either side of the impact to run the function...\n")
	time.window <- c(80, 50)
	vector_window <- set.time.window(delta_df[[1]], time.window)
	expect_true(all(unlist(lapply(vector_window, nrow)) == 4))## 2 datapoints either side
	expect_true(min(vector_window$time) == 50)
	expect_true(max(vector_window$time) == 80)


	time.window <- 0.3
	prop_window <- set.time.window(delta_df[[1]], time.window)
	expect_true(min(prop_window$time) == 40)
	expect_true(max(prop_window$time) == 80)

	### test make.deltatronic #### 
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL,dimension.level, n.matrix ) ## test without time.window
	expect_equal(names(delta_df), "66")
	expect_true(all(diff(delta_df$`66`[[1]]$time_elapsed)>0)) ## check time elapsed is increasing
	expect_true(all(diff(delta_df$`66`[[1]]$time)<0)) ## check raw time is decreasing
	expect_true(all(diff(delta_df$`66`[[1]]$time_post_cp)>=0))
	expect_equal(nrow(delta_df$`66`[[1]]$time), length(disparity$subsets))
	expect_equal(nrow(delta_df$`66`[[1]]$disparity), length(disparity$subsets))
    expected_impact <- ifelse(delta_df$`66`[[1]]$time <= changepoint, 1, 0)
    expect_equal(as.vector(delta_df$`66`[[1]]$impact), as.vector(expected_impact))
    expect_true(any(delta_df$`66`[[1]]$impact == 0))
    expect_true(any(delta_df$`66`[[1]]$impact == 1))
    first_impact_index <- min(which(delta_df$`66`[[1]]$time <= changepoint))
    expect_equal(as.numeric(delta_df$`66`[[1]]$impact[first_impact_index, ]), 1)
    expect_equal(as.numeric(delta_df$`66`[[1]]$impact[first_impact_index - 1, ]), 0)

	delta_df <- make.deltatronic(disparity, 66, time.window = 3, dimension.level, n.matrix) ## test without time.window
	expect_true(all(unlist(lapply(delta_df$`66`[[1]], nrow)) == 6)) ## 3 datapoints either sie
	expect_true(sum(delta_df$`66`[[1]]$impact == 0) == sum(delta_df$`66`[[1]]$impact == 1)) ## equal number of 0 and 1

	expect_is(delta_df, "list")
	expect_is(delta_df[[1]], "list")
	expect_is(delta_df[[1]][[1]], "list")
	expect_true(all(unlist(lapply(delta_df, lapply, lapply,is.matrix))))
	expect_true(all(names(delta_df$`66`[[1]]) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp")))	
	# delta_df <- make.deltatronic(disparity, 66, time.window = NULL) ## no error

	## multi dim disparity	
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
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL, dimension.level = 5, n.matrix = n.matrix)
	expect_equal(names(delta_df), as.character(changepoint))
	expect_true(all(diff(delta_df[[1]][[1]]$time_elapsed)>0)) ## check time elapsed is increasing
	expect_true(all(diff(delta_df[[1]][[1]]$time)<0)) ## check raw time is decreasing
	expect_true(all(diff(delta_df[[1]][[1]]$time_post_cp)>=0))
	expect_equal(nrow(delta_df[[1]][[1]]$time), length(disparity$subsets))
	expect_equal(nrow(delta_df[[1]][[1]]$disparity), length(disparity$subsets))
    expected_impact <- ifelse(delta_df[[1]][[1]]$time <= changepoint, 1, 0)
    expect_equal(as.vector(delta_df[[1]][[1]]$impact), as.vector(expected_impact))
    expect_true(any(delta_df[[1]][[1]]$impact == 0))
    expect_true(any(delta_df[[1]][[1]]$impact == 1))
    first_impact_index <- min(which(delta_df[[1]][[1]]$time <= changepoint))
    expect_equal(as.numeric(delta_df[[1]][[1]]$impact[first_impact_index, ]), 1)
    expect_equal(as.numeric(delta_df[[1]][[1]]$impact[first_impact_index - 1, ]), 0)



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
	data <- dispRity(data, metric = c(sum,variances))


    n.matrix <- 1
    if (length(data$matrix) > 1){
        n.matrix  <- length(data$matrix)
    }

    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }

	delta_df <- make.deltatronic(data, changepoint, time.window = NULL, dimension.level, n.matrix)
	#@@@ test on multi.matrix
	expect_equal(names(delta_df), as.character(changepoint))
	expect_true(all(diff(delta_df[[1]][[1]]$time_elapsed)>0)) ## check time elapsed is increasing
	expect_true(all(diff(delta_df[[1]][[1]]$time)<0)) ## check raw time is decreasing
	expect_true(all(diff(delta_df[[1]][[1]]$time_post_cp)>=0))
	expect_equal(nrow(delta_df[[1]][[1]]$time), length(disparity$subsets))
	expect_equal(nrow(delta_df[[1]][[1]]$disparity), length(disparity$subsets))
    expected_impact <- ifelse(delta_df[[1]][[1]]$time <= changepoint, 1, 0)
    expect_equal(as.vector(delta_df[[1]][[1]]$impact), as.vector(expected_impact))
    expect_true(any(delta_df[[1]][[1]]$impact == 0))
    expect_true(any(delta_df[[1]][[1]]$impact == 1))
    first_impact_index <- min(which(delta_df[[1]][[1]]$time <= changepoint))
    expect_equal(as.numeric(delta_df[[1]][[1]]$impact[first_impact_index, ]), 1)
    expect_equal(as.numeric(delta_df[[1]][[1]]$impact[first_impact_index - 1, ]), 0)

	expect_equal(as.numeric(delta_df[[1]][[1]]$disparity[1,]), get.disparity(data, concatenate = FALSE)[[1]][1] ) ## check the values are extracted in correct order
	expect_equal(as.numeric(delta_df[[1]][[10]]$disparity["2",]), get.disparity(data, concatenate = FALSE)$`2`[10] )
	expect_equal(as.numeric(delta_df[[1]][[8]]$disparity["5",]), get.disparity(data, concatenate = FALSE)$`5`[8] )



	delta_df <- make.deltatronic(data, changepoint, time.window = 3, dimension.level, n.matrix ) ## test without time.window
	expect_true(all(unlist(lapply(delta_df[[1]][[1]], nrow)) == 6)) ## 3 datapoints either sie
	expect_true(sum(delta_df[[1]][[1]]$impact == 0) == sum(delta_df[[1]][[1]]$impact == 1)) ## equal number of 0 and 1

	expect_is(delta_df, "list")
	expect_is(delta_df[[1]][[1]], "list")
	expect_true(all(unlist(lapply(delta_df[[1]], lapply, is.matrix))))
	expect_true(all(names(delta_df[[1]][[1]]) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp")))	
	# delta_df <- make.deltatronic(disparity, 66, time.window = NULL) ## no error
	expect_equal(as.numeric(delta_df[[1]][[10]]$disparity["2",]), get.disparity(data, concatenate = FALSE)$`2`[10] )
	expect_equal(as.numeric(delta_df[[1]][[8]]$disparity["5",]), get.disparity(data, concatenate = FALSE)$`5`[8] )



	
	## multi and multidimensional matrix
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
	data <- dispRity(data, metric = c(variances))


    n.matrix <- 1
    if (length(data$matrix) > 1){
        n.matrix  <- length(data$matrix)
    }

    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }


	delta_df <- make.deltatronic(data, changepoint, time.window = NULL, dimension.level, n.matrix)
	#@@@ test on multi.matrix
	expect_equal(names(delta_df), as.character(changepoint))
	expect_true(all(diff(delta_df[[1]][[1]]$time_elapsed)>0)) ## check time elapsed is increasing
	expect_true(all(diff(delta_df[[1]][[1]]$time)<0)) ## check raw time is decreasing
	expect_true(all(diff(delta_df[[1]][[1]]$time_post_cp)>=0))
	expect_equal(nrow(delta_df[[1]][[1]]$time), length(disparity$subsets))
	expect_equal(nrow(delta_df[[1]][[1]]$disparity), length(disparity$subsets))
    expected_impact <- ifelse(delta_df[[1]][[1]]$time <= changepoint, 1, 0)
    expect_equal(as.vector(delta_df[[1]][[1]]$impact), as.vector(expected_impact))
    expect_true(any(delta_df[[1]][[1]]$impact == 0))
    expect_true(any(delta_df[[1]][[1]]$impact == 1))
    first_impact_index <- min(which(delta_df[[1]][[1]]$time <= changepoint))
    expect_equal(as.numeric(delta_df[[1]][[1]]$impact[first_impact_index, ]), 1)
    expect_equal(as.numeric(delta_df[[1]][[1]]$impact[first_impact_index - 1, ]), 0)

	expect_equal(as.numeric(delta_df[[1]][[1]]$disparity[1,1]), get.disparity(data, concatenate = FALSE)[[1]][,1][1] ) ## check the values are extracted in correct order
	expect_equal(as.numeric(delta_df[[1]][[10]]$disparity["2",3]), get.disparity(data, concatenate = FALSE)$`2`[3,10] ) ## this is at t = 2, in the 10th matrix, 3rd dimension
	expect_equal(as.numeric(delta_df[[1]][[4]]$disparity["7", 1]), get.disparity(data, concatenate = FALSE)$`7`[1,4] )
})


test_that("average.method works", {
	## TODO caleb
	data(disparity)
	n.matrix <- 1
    if (length(disparity$matrix) > 1){
        n.matrix  <- length(disparity$matrix)
    }

    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }

	delta_df <- make.deltatronic(disparity, 66, time.window = NULL,dimension.level, n.matrix)
	average <- lapply(delta_df, lapply, average.method, dimension.level = dimension.level)#
	expect_is(average[[1]][[1]], "htest")
	expect_equal(average[[1]][[1]]$method, "Welch Two Sample t-test")
	average <- lapply(delta_df, lapply, average.method, alternative = "less", dimension.level = dimension.level)#
	expect_equal(average[[1]][[1]]$alternative, "less")
	average <- lapply(delta_df, lapply, average.method, aov,dimension.level= dimension.level)
	expect_is(average[[1]][[1]], "aov")
	data(disparity)
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL, dimension.level, n.matrix )
	average <- lapply(delta_df, lapply, average.method, wilcox.test, dimension.level = dimension.level)#
	expect_equal(average[[1]][[1]]$method, "Wilcoxon rank sum exact test")
	average <- lapply(delta_df, lapply, average.method, wilcox.test, alternative = "less", dimension.level = dimension.level)#
	expect_equal(average[[1]][[1]]$alternative, "less")


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
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL, dimension.level = 5, n.matrix)
	average <- lapply(delta_df, lapply, average.method, wilcox.test, dimension.level = dims, n.matrix)#
	expect_equal(dims, length(average[[1]][[1]]))
}
)

test_that("itsa.method works", {
	data(disparity)

	n.matrix <- 1
    if (length(disparity$matrix) > 1){
        n.matrix  <- length(disparity$matrix)
    }

    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }


	delta_df <- make.deltatronic(disparity, 66, time.window = NULL, dimension.level, n.matrix )
	method <- lapply(delta_df, lapply, itsa.method, dimension.level)#
	expect_is(method[[1]][[1]], "list")
	expect_true(all(names(method[[1]][[1]]) %in% c("data", "model")))
	expect_true(all(names(method[[1]][[1]]$data) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp", "counter_mean_ci", "counter_lower_ci", "counter_upper_ci")))
	expect_is(method[[1]][[1]]$model, "lm")

	## test on multidim disparity
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
		n.matrix <- 1
    if (length(data$matrix) > 1){
        n.matrix  <- length(data$matrix)
    }
    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL , dimension.level, n.matrix)
	dims <- max(data$call$dimensions)
	method <- lapply(delta_df, lapply, itsa.method, dimension.level = dims)#
	expect_is(method[[1]][[1]], "list")
	expect_true(all(names(method[[1]][[1]]) %in% c("data", "model")))
	expect_true(all(names(method[[1]][[1]]$data) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp", "counter_mean_ci", "counter_lower_ci", "counter_upper_ci")))
	expect_is(method[[1]][[1]]$model[[1]], "lm")

}
)

# test_that("calculate.slope.effect works", {
# 	## test for no change
# 	set_state <- set.seed(123)
# 	time_elapsed <- seq(0, 60, by = 5)
# 	baseline_slope <- 0.002
# 	intercept      <- 1.82
# 	noise <- rnorm(length(time_elapsed), mean = 0, sd = 0.005)
# 	disparity <- intercept + (baseline_slope * time_elapsed) + noise
# 	delta_df <- list(
# 	time_elapsed = as.matrix(time_elapsed),
# 	disparity    = as.matrix(disparity),
# 	impact       = as.matrix(ifelse(time_elapsed >= 30, 1, 0)),
# 	time_post_cp = as.matrix(ifelse(time_elapsed > 30, time_elapsed - 30, 0))
# 	)

# 	itsa <- itsa.method(delta_df, dimension.level = 1)

# 	calculate.slope.effect(itsa)

# 	set_state <- set.seed(123)
# 	time_elapsed <- seq(0, 60, by = 5)

# 	intercept      <- 1.82
# 	baseline_slope <- 0.002
# 	slope_change   <- 0.0005  

# 	time_post_cp_vec <- ifelse(time_elapsed > 30, time_elapsed - 30, 0)

# 	noise <- rnorm(length(time_elapsed), mean = 0, sd = 0.005)
# 	disparity <- intercept + (baseline_slope * time_elapsed) + (slope_change * time_post_cp_vec) + noise

# 	delta_df <- list(
# 	time_elapsed = as.matrix(time_elapsed),
# 	disparity    = as.matrix(disparity),
# 	impact       = as.matrix(ifelse(time_elapsed >= 30, 1, 0)),
# 	time_post_cp = as.matrix(time_post_cp_vec)
# 	)

# 	itsa <- itsa.method(delta_df, dimension.level = 1)

# 	plot(disparity ~ time_elapsed, data = delta_df, type = "l")

# 	print(calculate.slope.effect(itsa))

# 	data(disparity)
# 	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
# 	method <- lapply(delta_df, itsa.method, dimension.level = 1)#
# 	expect_is(method[[1]], "list")
# 	expect_true(all(names(method[[1]]) %in% c("data", "model")))
# 	expect_true(all(names(method[[1]]$data) %in% c("time", "time_elapsed", "impact", "disparity", "time_post_cp", "counter_mean_ci", "counter_lower_ci", "counter_upper_ci")))
# 	expect_is(method[[1]]$model, "lm")

# }
# )

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
	n.matrix <- length(disparity$matrix)

	dimension.level <- 1
    if (any(unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }

	changepoint <- 66
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL, dimension.level, n.matrix)
	changepoint <- set.changepoint(changepoint)
	nsim <- 10
	control <- lapply(changepoint, make.control, data = disparity, paint = TRUE, nsim = nsim, n.matrix  = n.matrix)
	expect_is(control, "list")
	expect_equal(names(control), "66")
	expect_equal(length(control[[1]][[1]]$subsets), length(disparity$subsets))
	expect_equal(length(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]]), length(get.disparity(disparity)[[1]]) * 10)
	error <- capture_error(lapply(changepoint, make.control, data = disparity, paint = FALSE, nsim = nsim, n.matrix = n.matrix))
	expect_equal(error[[1]], "`slice.model` argument needs to be inputted if paint = FALSE...\n")
	error <- capture_error(lapply(changepoint, make.control, data = disparity, paint = FALSE, slice.model = 5, nsim = nsim))
	expect_equal(error[[1]], "slice.model argument must be one of the following: acctran, deltran, random, proximity, equal.split, gradual.split.")
	expect_equal(names(control[[1]][[1]]), c("matrix" ,    "tree"   ,    "call"    ,   "subsets"   , "disparity"  ,"sim_params"))
	expect_equal(ncol(control[[1]][[1]]$sim_params),ncol(get.matrix(disparity)))
	expect_equal(nrow(get.matrix(control[[1]][[1]])), nrow(get.matrix(disparity)))


	control_deltatronic <- make.ctrl.deltatronic(control, changepoint, time.window = NULL, dimension.level= dimension.level, nsim = nsim)
	expect_equal(names(control_deltatronic[[1]][[1]][[1]]), c("time", "time_elapsed", "impact", "disparity", "time_post_cp"))
	expect_equal(length(control_deltatronic$`66`[[1]]), length(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]])) ## test that 10 sims have correctly formatted to control_deltatronic with list of 10


	## multi dim
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

	n.matrix <- length(data$matrix)

    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }
    # dimension.level <- as.integer(gsub("level", "", levels))[1]
	delta_df <- make.deltatronic(data, changepoint, time.window = NULL, dimension.level, n.matrix)
	dims <- max(data$call$dimensions)
	changepoint <- set.changepoint(changepoint)
    control <- lapply(changepoint, make.control, data = data, nsim = nsim, n.matrix = n.matrix)
	expect_is(control, "list")
	expect_equal(names(control), "3.61339478986338")
	expect_equal(length(control[[1]][[1]]$subsets), length(disparity$subsets))
	expect_equal(length(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]]), length(get.disparity(data)[[1]]) * nsim)
	error <- capture_error(lapply(changepoint, make.control, data, paint = FALSE, nsim = nsim, n.matrix = n.matrix))
	expect_equal(error[[1]], "`slice.model` argument needs to be inputted if paint = FALSE...\n")
	error <- capture_error(lapply(changepoint, make.control, data, paint = FALSE, slice.model = 5, n.matrix =  n.matrix, nsim = nsim))
	expect_equal(error[[1]], "slice.model argument must be one of the following: acctran, deltran, random, proximity, equal.split, gradual.split.")
	expect_equal(names(control[[1]][[1]]), c("matrix" ,    "tree"   ,    "call"    ,   "subsets"   , "disparity"  ,"sim_params"))
	expect_equal(ncol(control[[1]][[1]]$sim_params), ncol(get.matrix(data)))
	expect_equal(nrow(get.matrix(control[[1]][[1]])), nrow(get.matrix(data)))

	## testing make.deltatronic works with control input
	control_deltatronic <- make.ctrl.deltatronic(control, changepoint, time.window= NULL, dimension.level= dimension.level,nsim = nsim)
	expect_equal(names(control_deltatronic[[1]][[1]][[1]]), c("time", "time_elapsed", "impact", "disparity", "time_post_cp"))
	expect_equal(ncol(control_deltatronic$`3.61339478986338`[[1]][[1]]$disparity), nrow(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]])) ## test that 5 dimensions have correctly formatted to control_deltatronic
	expect_equal(length(control_deltatronic$`3.61339478986338`[[1]]), ncol(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]])) ## test that 10 sims have correctly formatted to control_deltatronic
	expect_equal(as.numeric(control_deltatronic[[1]][[1]][[10]]$disparity["2",1]), get.disparity(control[[1]][[1]], concatenate = FALSE)$`2`[1, 10])
	expect_equal(as.numeric(control_deltatronic[[1]][[1]][[8]]$disparity["5",4]), get.disparity(control[[1]][[1]], concatenate = FALSE)$`5`[4, 8])


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
	data <- dispRity(data, metric = c(sum,variances))

	n.matrix <- length(data$matrix)

    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }

	changepoint <- set.changepoint(changepoint)

	nsim  <- 12 ## switch up nsim 
	control <- lapply(changepoint, make.control, data = data, nsim = nsim, paint = TRUE, n.matrix =n.matrix)
	control_deltatronic <- make.ctrl.deltatronic(control, changepoint, time.window= NULL, dimension.level= dimension.level,nsim = nsim)
	expect_equal(names(control_deltatronic[[1]][[1]][[1]]), c("time", "time_elapsed", "impact", "disparity", "time_post_cp"))
	expect_equal(length(control_deltatronic$`3.61339478986338`[[1]]), length(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]])) ## test that 12 sims have correctly formatted to control_deltatronic
	expect_equal(length(control_deltatronic$`3.61339478986338`), 10) ## 10 matrices matches
	# expect_equal(length(control_deltatronic$`3.61339478986338`[[1]]), ncol(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]]))
	expect_equal(as.numeric(control_deltatronic[[1]][[1]][[10]]$disparity["2",]), get.disparity(control[[1]][[1]], concatenate = FALSE)$`2`[10])
	expect_equal(as.numeric(control_deltatronic[[1]][[1]][[8]]$disparity["5",]), get.disparity(control[[1]][[1]], concatenate = FALSE)$`5`[8])


	
	## multi and multidimensional matrix
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
	disp <- dispRity(data, metric = c(variances))
	dimension.level <- 1
    if (any(unlist(lapply(get.disparity(disp, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(disp, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }


	nsim <- 12
	delta_df <- make.deltatronic(disp, changepoint, time.window = NULL, n.matrix = 10, dimension.level = dimension.level)
	changepoint <- set.changepoint(changepoint)
    control <- lapply(changepoint, make.control, data = disp, nsim = nsim, n.matrix = 10)

	control_deltatronic <- make.ctrl.deltatronic(control, changepoint, time.window= NULL, dimension.level= dimension.level,nsim = nsim)
	expect_equal(names(control_deltatronic[[1]][[1]][[1]]), c("time", "time_elapsed", "impact", "disparity", "time_post_cp"))
	expect_equal(length(control_deltatronic$`3.61339478986338`[[1]]), ncol(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]])) ## test that 12 sims have correctly formatted to control_deltatronic
	expect_equal(length(control_deltatronic$`3.61339478986338`), 10) ## 10 matrices matches
	# expect_equal(length(control_deltatronic$`3.61339478986338`[[1]]), ncol(get.disparity(control[[1]][[1]], concatenate = FALSE)[[1]]))
	expect_equal(as.numeric(control_deltatronic[[1]][[1]][[10]]$disparity["2",3]), get.disparity(control[[1]][[1]], concatenate = FALSE)$`2`[3,10])
	expect_equal(as.numeric(control_deltatronic[[1]][[1]][[8]]$disparity["5", 4]), get.disparity(control[[1]][[1]], concatenate = FALSE)$`5`[4,8])


}
)


test_that("citsa.method works...\n", {
	data(disparity)
	nsim <- 12
	cp <- 66
	dimension.level <- 1
    if (any(unlist(lapply(get.disparity(disparity, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }
	n.matrix <- length(disparity$matrix)
	delta_df <- make.deltatronic(disparity, cp, time.window =NULL, dimension.level, n.matrix)
	changepoint <- set.changepoint(cp)

    control <- lapply(changepoint, make.control, data = disparity, nsim = nsim, n.matrix= n.matrix)
	control_deltatronic <- make.ctrl.deltatronic(control, changepoint, time.window = NULL, dimension.level, nsim = nsim)
	control_delta_df <- lapply(control_deltatronic, lapply, lapply, function(x) {
                x$emp_vs_null <- matrix(0, nrow = nrow(x$time))
                return(x)
    })

	delta_df <- lapply(delta_df, lapply, function(x) {
            x$emp_vs_null <- matrix(1, nrow = nrow(x$time))
            return(x)
            })

	
	full_df <- bind.delta(delta_df, control_delta_df, dimension.level = dimension.level )
	
	expect_is(full_df, "list")
	expect_is(full_df[[1]][[1]], "list")
	expect_is(full_df[[1]][[1]][[1]], "data.frame")

	# expect_equal(subset(full_df[[1]][[1]][[1]], emp_vs_null ==1)$disparity, as.numeric(delta_df[[1]][[1]]$disparity)) ## tests empirical disparity matches original delta_df
	# expect_equal(subset(full_df[[1]][[1]][[5]], emp_vs_null ==0)$disparity, as.numeric(control_delta_df[[1]][[1]][[5]]$disparity))
	expect_equal(unique(unlist(lapply(full_df, lapply, lapply, nrow))), nrow(delta_df[[1]][[1]][[1]]) *2) ## should be double the number of rows

	#
	# now test citsa.method works

	citsa <- lapply(full_df, lapply, lapply, citsa.method)
	## test structure of citsa now
	expect_is(citsa, "list")
	expect_is(citsa[[1]], "list")
	expect_is(citsa[[1]][[1]], "list")
	expect_is(citsa[[1]][[1]][[1]], "list")
	expect_named(citsa[[1]][[1]][[1]], c("model", "control_slope_change", "emp_slope_change", "data"))
	expect_is(citsa[[1]][[1]][[1]]$data, "data.frame")
	

	## multi matrix
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
	data <- dispRity(data, metric = c(sum,variances))

	n.matrix <- length(data$matrix)

    dimension.level <- 1
    if (any(unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }
	delta_df <- make.deltatronic(data, changepoint, time.window =NULL, dimension.level, n.matrix)

	changepoint <- set.changepoint(changepoint)
	control <- lapply(changepoint, make.control, data = data, nsim = nsim, n.matrix= n.matrix)
	control_deltatronic <- make.ctrl.deltatronic(control, changepoint, time.window = NULL, dimension.level, nsim = nsim)
	control_delta_df <- lapply(control_deltatronic, lapply, lapply, function(x) {
				x$emp_vs_null <- matrix(0, nrow = nrow(x$time))
				return(x)
	})

	delta_df <- lapply(delta_df, lapply, function(x) {
			x$emp_vs_null <- matrix(1, nrow = nrow(x$time))
			return(x)
			})

	full_df <- bind.delta(delta_df, control_delta_df, dimension.level = dimension.level )


})




## eg multi.ace sample = >1 output
# test_that("multi matrix disparity works", {
# 	set.seed(123)
# 	tree <- rtree(n = 100)
# 	tree <- makeNodeLabel(tree)
# 	tree <- set.root.time(tree)
# 	changepoint <- tree$root.time / 2
# 	mat_1 <- matrix(rnorm(995), 199, 5)
# 	mat_2 <- matrix(rnorm(995), 199, 5)
# 	rownames(mat_2)	 <- rownames(mat_1) <- c(tree$tip.label, tree$node.label)

# 	mat_2[grepl("^t", rownames(mat_2)), ] <- mat_1[grepl("^t", rownames(mat_1)), ] ## both tips have same

# 	multi_data <- make.dispRity(list(mat_1, mat_2), tree)
# 	multi_data <- chrono.subsets(multi_data, method = "c", model = "equal.split", time = c(7,6,5,4,3,2,1), inc.nodes = TRUE)



# 	data <- dispRity(multi_data, c(sum, variances))
# 	n.matrix <- length(data$matrix)


# 	delta_df <- make.deltatronic(data, changepoint, time.window = NULL, dimension.level = 1, n.matrix)
# 	dims <- max(data$call$dimensions)
# 	changepoint <- set.changepoint(changepoint)
#     control <- lapply(changepoint, make.control, data = data, nsim = nsim)


	
# 	var.fun <- function(mat){
# 		variances(mat)
# 	}
# 	multi_multi_dimensional <- dispRity(multi_data, metric = var.fun)



# })


test_that("chrono.test (method = `citsa`) works", {
	data(disparity)


	out <- chrono.test(disparity, method = "citsa", changepoint = 66, nsim = 50)


	## multi matrix
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
	data <- dispRity(data, metric = c(sum,variances))

	out <- chrono.test(data, method = "citsa", changepoint = 3.613395, nsim = 100)

	## test time.window
	out <- chrono.test(data, method = "citsa", changepoint = 3.613, nsim = 10, time.window = c(5, 1))

	out <- chrono.test(data, method = "itsa", changepoint = 3.613395, nsim = 10)

})




test_that("chrono.test (method = `average`) works", {
	data(disparity)
	# out <- chrono.test(disparity, method = "average", changepoint = 66,  test = stats::prop.test, n = 10)
	out <- chrono.test(disparity, method = "average", changepoint = 66, time.window = c(80,40))



})