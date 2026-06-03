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

})


test_that("average.method works", {
	## TODO caleb
	data(disparity)
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
	average <- lapply(delta_df, average.method)#
	expect_is(average[[1]], "htest")
	expect_equal(average[[1]]$method, "Welch Two Sample t-test")
	average <- lapply(delta_df, average.method, aov)
	expect_is(average[[1]], "aov")
}
)

test_that("itsa.method works", {
	data(disparity)
	delta_df <- make.deltatronic(disparity, 66, time.window = NULL)
	average <- lapply(delta_df, average.method)#

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
	expect_true(all(c("Pre_Intervention", "Post_Intervention") %in% names(unlist(painted$maps))))
	

	
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
	expect_equal(length(control[[1]]$subsets), length(disparity$subsets))
	expect_equal(length(get.disparity(control[[1]])[[1]]), length(get.disparity(disparity)[[1]]) * 10)

}
)