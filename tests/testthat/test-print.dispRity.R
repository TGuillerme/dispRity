#TESTING print.dispRity

context("print.dispRity") 

## Loading the data
data("disparity")
data("BeckLee_mat50")
data("BeckLee_mat99")
data("BeckLee_tree")


test_that("normal printing", {
    ## Empty object
    test <- make.dispRity() 
    expect_equal(capture.output(test), "Empty dispRity object.")

    ## Empty object with a matrix
    test <- make.dispRity(data = matrix(1))
    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "Contains only a matrix 1x1."
    ))

    ## Time subsets
    test <- chrono.subsets(BeckLee_mat50, time = c(100, 90, 50), method = "discrete", tree = BeckLee_tree)

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "2 discrete time subsets for 50 elements:",
        "    100 - 90, 90 - 50."
    ))

    test <- chrono.subsets(BeckLee_mat99, time = c(100,90,80,70,50,60,40), method = "continuous", tree = BeckLee_tree, model = "ACCTRAN")

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "7 continuous (acctran) time subsets for 99 elements:",
        "     100, 90, 80, 70, 50 ..."
    ))

    ## Custom subsets
    expect_warning(test <- custom.subsets(matrix(data = rnorm(90), nrow = 10), list(c(1:4), c(5:10))))

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "2 customised subsets for 10 elements:",
        "    1, 2."
    ))

    ## Bootstrapped data
    test <- boot.matrix(BeckLee_mat50)

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "50 elements with 48 dimensions.",
        "Data was bootstrapped 100 times (method:\"full\")."
    ))

    ## Bootstrapped + subsets
    test <- boot.matrix(chrono.subsets(BeckLee_mat50, time = c(100, 90, 50), method = "discrete", tree = BeckLee_tree))

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "2 discrete time subsets for 50 elements with 48 dimensions:",
        "    100 - 90, 90 - 50.",
        "Data was bootstrapped 100 times (method:\"full\")."
    ))

    ## Disparity only    
    test <- dispRity(BeckLee_mat50, metric = mean)

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "50 elements with 48 dimensions.",
        "Disparity was calculated as: mean."
    ))

    ## Bootstrapped + subsets + rarefaction + disparity

    expect_equal(capture.output(disparity), 
        c(
        " ---- dispRity object ---- ",
        "7 continuous (acctran) time subsets for 99 elements with 97 dimensions:",
        "     90, 80, 70, 60, 50 ...",
        "Data was bootstrapped 100 times (method:\"full\") and rarefied to 20, 15, 10, 5 elements.",
        "Disparity was calculated as: c(median, centroids)."
    ))

})

test_that("randtest printing", {
    set.seed(1)
    obs_disparity <- dispRity(BeckLee_mat50, metric = ellipse.volume)
    test <- null.test(obs_disparity, replicates = 100, null.distrib = rnorm)

    expect_equal(capture.output(test),
        c("Monte-Carlo test", "Call: [1] \"dispRity::null.test\"",
        "", "Observation: 6.634e-07 ", 
        "", "Based on 100 replicates", 
        "Simulated p-value: 0.03960396 ", "Alternative hypothesis: two-sided ", 
        "", "      Std.Obs   Expectation      Variance ",
        "-1.400160e-01  3.141577e+29  5.034313e+60 "))

})


test_that("dtt printing", {
    ## Loading geiger's example data set
    data <- matrix(rnorm(20), ncol = 2)
    rownames(data) <- paste0("t", 1:10)
    tree <- rtree(10)

    ## The average squared pairwise distance metric (used in geiger::dtt)
    average.sq <- function(X) mean(pairwise.dist(X)^2)
    ## Calculate the disparity of the dataset using dtt.dispRity
    dispRity_dtt <- dtt.dispRity(data = data[], metric = average.sq,
                                 tree = tree, nsim = 10)

    print_dtt <- capture.output(dispRity_dtt)

    expect_equal(print_dtt[length(print_dtt)-1],
        c("- attr(*, \"class\") = \"dispRity\" \"dtt\""))
    expect_equal(print_dtt[length(print_dtt)],
        c("Use plot.dispRity to visualise."))

})
