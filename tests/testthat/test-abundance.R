test_that("abundance is handle as a dispRity component", {

    ## Abundance data
    data(BeckLee_mat50) 
    data <- make.dispRity(data = BeckLee_mat50)
    abundance_data <- matrix(sample(c(0,1,2,3), 200, replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.1)), nrow = 50, ncol = 4)
    rownames(abundance_data) <- rownames(BeckLee_mat50)
    colnames(abundance_data) <- c("site1", "site2", "site3", "site4") 

    ## check.abundance
    one_disp <- data$matrix[[1]]
    one_abundance <- abundance_data

    ## check matching names work
    test_ab <- one_abundance
    rownames(test_ab) <- NULL
    error_rows <- capture_error(check.abundance.match(one_disp, test_ab))
    expect_equal(error_rows[[1]], "Abundance data must have row names.")

    test_ab <- one_abundance
    colnames(test_ab) <- NULL
    error_cols <- capture_error(check.abundance.match(one_disp, test_ab))
    expect_equal(error_cols[[1]], "Abundance data must have column names.")

    test_ab <- one_abundance
    test_ab <- test_ab[-1, ]
    error_n <- capture_error(check.abundance.match(one_disp, test_ab))
    expect_equal(error_n[[1]], "Both the abundance data and the trait data should have the same number of rows.")

    test_ab <- one_abundance
    rownames(test_ab)[c(1, 3, 8)] <- c("tust", "test", "tist")
    error_name <- capture_error(check.abundance.match(one_disp, test_ab))
    expect_equal(error_name[[1]], "The following rownames in the abundance data are not matching the ones in the trait data: tust, test, tist.")

    ## General checking works
    test <- check.abundance(data$matrix, one_abundance)
    expect_is(test, "list")
    expect_equal(names(test), "matrix")
    expect_equal(length(test), 1)

    ## wrong abundance data
    error <- capture_error(check.abundance(data$matrix, "one_abundance"))
    expect_equal(error[[1]], "abundance_data must be of class matrix or list or data.frame.")

    ## Works with lists
    test <- check.abundance(data$matrix, list("matrix" = one_abundance))
    expect_is(test, "list")
    expect_equal(names(test), "matrix")
    expect_equal(length(test), 1)

    ## Wrong names in matrix
    test_ab <- one_abundance
    rownames(test_ab)[42] <- "bla"
    error <- capture_error(check.abundance(data$matrix, test_ab))
    expect_equal(error[[1]], "The following rownames in the abundance data are not matching the ones in the trait data: bla.")

    ## Wrong names in list
    error <- capture_error(check.abundance(data$matrix, list("matrix" = test_ab)))
    expect_equal(error[[1]], "The following rownames in the abundance data are not matching the ones in the trait data: bla.")

    ## Works in make.dispRity
    data <- make.dispRity(data = BeckLee_mat50)
    expect_equal(names(data), c("matrix", "tree", "call", "subsets"))
    error <- capture_error(data <- make.dispRity(data = BeckLee_mat50, abundance = "ha"))
    expect_equal(error[[1]], "abundance_data must be of class matrix or list or data.frame.")
    data <- make.dispRity(data = BeckLee_mat50, abundance = abundance_data)
    expect_equal(names(data), c("matrix", "tree", "call", "subsets", "abundance"))
    print <- capture_output(print(data))
    expect_equal(print[[1]], " ---- dispRity object ---- \nContains a matrix 50x48 with 1 associated abundance matrix.")

    data <- make.dispRity(data = list(BeckLee_mat50,BeckLee_mat50), abundance = abundance_data)
    expect_equal(names(data), c("matrix", "tree", "call", "subsets", "abundance"))
    print <- capture_output(print(data))
    expect_equal(print[[1]], " ---- dispRity object ---- \nContains 2 matrices 50x48 with 1 associated abundance matrix.")

    abundance_datas <- list(abundance_data, abundance_data)
    data <- make.dispRity(data = list(BeckLee_mat50,BeckLee_mat50), abundance = abundance_datas)
    expect_equal(names(data), c("matrix", "tree", "call", "subsets", "abundance"))
    print <- capture_output(print(data))
    expect_equal(print[[1]], " ---- dispRity object ---- \nContains 2 matrices 50x48 with 2 associated abundance matrices.")

    ## fill.dispRity
    data <- make.dispRity(data = BeckLee_mat50)
    test <- fill.dispRity(data, abundance = abundance_data)
    expect_equal(names(test), c("matrix", "tree", "call", "subsets", "abundance"))

    ## remove.dispRity
    data <- remove.dispRity(test, what = "abundance")
    expect_equal(names(data), c("matrix", "tree", "call", "subsets"))

    ## get.matrix
    abundance_datas <- list(abundance_data, abundance_data)
    abundance_datas[[2]][,4] <- 10
    data <- make.dispRity(data = list(BeckLee_mat50,BeckLee_mat50), abundance = abundance_datas)
    diti <- make.dispRity(BeckLee_mat50)
    error <- capture_error(get.matrix(diti, abundance = TRUE))
    expect_equal(error[[1]], "diti does not contain abundance data.")
    error <- capture_error(get.matrix(data, abundance = TRUE, matrix = c(1,3)))
    expect_equal(error[[1]], "data contains less than 3 abundance matrices.")
    test <- get.matrix(data, abundance = TRUE)
    expect_equal(test, abundance_data)
    test <- get.matrix(data, abundance = TRUE, matrix = 2)
    expect_equal(test, abundance_datas[[2]])
    test <- get.matrix(data, abundance = TRUE, matrix = c(1,2))
    expect_equal(test, abundance_datas)
})


test_that("abundance works for calculating dispRity metrics", {

#     ## Something like metric <- function(matrix, abundance)
#     data(BeckLee_mat50) 
#     abundance_data <- matrix(sample(c(0,1,2,3), 200, replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.1)), nrow = 50, ncol = 4)
#     rownames(abundance_data) <- rownames(BeckLee_mat50)
#     colnames(abundance_data) <- c("site1", "site2", "site3", "site4") 
#     data <- make.dispRity(data = BeckLee_mat50, abundance = abundance_data)


#     ## Standard test
#     average1 <- function(matrix, ...) {
#         sum(matrix)/length(matrix)
#     }
#     test1 <- dispRity(data, metric = average1)
#     print <- capture_output(print(test1))
#     expect_equal(print[[1]], " ---- dispRity object ---- \n50 elements in one matrix with 48 dimensions with 1 associated abundance matrix.\nDisparity was calculated as: average1.")
#     expect_equal_round(get.disparity(test1)[[1]], 3.918862e-17, 17)

 
#     ## Abundance only
#     average2 <- function(abundance, ...) {
#         sum(abundance)/length(abundance)
#     }    
#     test1 <- dispRity(data, metric = average2)
#     expect_equal(get.disparity(test1)[[1]], mean(abundance_data))


#     ## Switch to abundance
#     test1 <- dispRity(data, metric = as.abundance(average1))






# ## Metric fun:
#     # 1- abundance only metric  -> detected if args are only abundance
# average1 <- function(matrix, ...) {
#     sum(matrix)/length(matrix)
# }

# ## Toggling to abundance
# average2 <- as.abundance(average1)
# ## becomes:
# average2 <- function(abundance, ...) {
#     sum(abundance)/length(abundance)
# }
#     # 2- abundance + trait metric -> detected if args are matrix + abundance (+ tree + matrix2).
# abundance.average <- function(matrix, abundance, ...) {
#     sum(matrix * abundance)/ length(matrix)
# }

# ## metric 1
# dispRity(some_data, metric = as.abundance(average1))
# dispRity(some_data, metric = average2)

# # metric 2
# dispRity(some_data, metric = abundance.average)

# ## Needs checking
#     # 1- if metric has abundance, does data has abundance?




})


test_that("abundance works for with the other dispRity components", {
    ## custom.subsets
    ## chrono.subsets
}) 