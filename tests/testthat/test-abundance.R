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
    


 # For make.dispRity

 # remove.dispRity

 # fill.dispRity

 # get.matrix (add abundance = TRUE and just work on that)

})

test_that("abundance is printed")