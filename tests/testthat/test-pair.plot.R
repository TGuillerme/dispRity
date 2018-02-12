#TESTING plot.dispRity

context("pair.plot") 

## Loading the data
data("disparity")

## Finding number of elements
test_that("find.num.elements internal", {
    expect_error(find.num.elements(nrow(matrix(NA, 5, 5))))
    expect_equal(find.num.elements(nrow(matrix(data = runif(42), ncol = 2))), 7)
})


## Testing the examples
test_that("testing examples", {

    data <- matrix(data = runif(42), ncol = 2)
    test <- pair.plot(data, what = 1, col = c("orange", "blue"), legend = TRUE, diag = 1)
    
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    pair.plot(data, what = 1, col = c("orange", "blue"), legend = TRUE, diag = 1)
    test <- pair.plot(data, what = 2, binary = 0.2, add = "*", cex = 2)
    expect_null(test)
  
    data(disparity)
    tests <- test.dispRity(disparity, test = wilcox.test, correction = "bonferroni")
    test <- pair.plot(as.data.frame(tests), what = "p.value", binary = 0.05)
    expect_null(test)
})
