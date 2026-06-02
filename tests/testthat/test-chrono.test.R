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
	changepoint  <- 69
	changepoint  <- set.changepoint(changepoint)
	expect_is(changepoint, "list")
	expect_equal(as.numeric(names(changepoint)), changepoint[[1]])
	changepoint <- "detect"

})