context("sequential.test")

#Testing data
data(BeckLee_mat50)
groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)),
     dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
sum_of_variances <- dispRity(boot.matrix(custom.subsets(BeckLee_mat50, groups), bootstraps = 100), metric = c(sum, variances))
subsets <- extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE)
seq_subsets <- list(c(1,2), c(2,3))

test_that("set.pair.subsets internal", {
    set.seed(1)
    subsets_pair <- list(replicate(3,rnorm(10), simplify = FALSE), replicate(3,rnorm(10, 100), simplify = FALSE))
    #Errors
    expect_error(
        set.pair.subsets("a", NULL)
        )
    expect_error(
        set.pair.subsets(list(), NULL)
        )

    #Normal results
    expect_is(
        set.pair.subsets(subsets_pair, intercept = NULL)
        ,"list")
    expect_equal(
        unique(unlist(lapply(set.pair.subsets(subsets_pair, intercept = NULL), class)))
        ,"data.frame")
    expect_equal(
        unique(unlist(lapply(set.pair.subsets(subsets_pair, intercept = NULL), dim)))
        ,c(20, 2))
    expect_equal(
        unique(unlist(lapply(set.pair.subsets(subsets_pair, intercept = "a"), dim)))
        ,c(20, 3))
    expect_lt(
        max(set.pair.subsets(subsets_pair, intercept = NULL)[[1]][[1]][1:10])
        ,50)
    expect_gt(
        min(set.pair.subsets(subsets_pair, intercept = NULL)[[1]][[1]][11:20])
        ,50)
    expect_equal(
        unique(set.pair.subsets(subsets_pair, intercept = NULL)[[1]][[2]][1:10])
        ,0)
    expect_equal(
        unique(set.pair.subsets(subsets_pair, intercept = NULL)[[1]][[2]][11:20])
        ,1)
    expect_false(
        set.pair.subsets(subsets_pair, intercept = NULL)[[1]][[1]][1] == set.pair.subsets(subsets_pair, intercept = NULL)[[2]][[1]][1]
        )
})

test_that("intercept.estimate works", {
    #Errors
    expect_error(
        intercept.estimate(intercept0 = "a", slope = 2)
        )
    expect_error(
        intercept.estimate(intercept0 = 1, slope = "a")
        )
    #Normal results
    expect_is(
        intercept.estimate(intercept0 = 1, slope = 2)
        , "numeric")
    expect_equal(
        intercept.estimate(intercept0 = 2, slope = 3)
        , 2 + 3 * 1)
    expect_equal(
        intercept.estimate(intercept0 = 2, slope = c(3,3))
        , 8)
})

test_that("set.intercept.next internal", {
    #glm example
    counts <- c(18,17,15,20,10,20,25,13,12) ; outcome <- gl(3,1,9) ; treatment <- gl(3,3)
    d.AD <- data.frame(treatment, outcome, counts)
    glm.D93 <- glm(counts ~ outcome, family = poisson())
    
    #Errors
    expect_error(
        set.intercept.next(d.AD)
        )
    
    #Calculate the right intercept
    expect_equal(
        set.intercept0(glm.D93)
        ,coef(glm.D93)[1])
    #Does for a model with more than one term!
    # expect_equal( 
    #     set.intercept.next(glm.D93, coef(glm.D93)[1])
    #     ,coef(glm.D93)[1] + coef(glm.D93)[2] * 1)
})

test_that("create.model works", {
    #Errors
    expect_error(
        create.model("a", family = gaussian)
        )
    expect_error(
        create.model(matrix(2,2), family = gaussian)
        )
    expect_error(
        create.model(set.pair.subsets(subsets[seq_subsets[[1]]]), family = "whatever")
        )
    #Normal results
    expect_is(
        create.model(set.pair.subsets(subsets[seq_subsets[[1]]])[[1]], family = gaussian)
        , c("glm","lm"))
    expect_equal(
        length(create.model(set.pair.subsets(subsets[seq_subsets[[1]]])[[1]], family = gaussian))
        , 30)
})

test_that("sequential.test works", {
    #Errors
    expect_warning(
        expect_error(
            sequential.test(sum_of_variances, family = gaussian)
            )
        )
    expect_error(
        sequential.test(subsets, family = c(1, 2))
        )
    #results
    test <- sequential.test(subsets, family = gaussian)
    expect_is(
        test
        , c("dispRity", "seq.test"))
    expect_equal(
        length(test$models)
        , 2)
    expect_equal(
        length(test$intercepts)
        , 2)
    expect_equal(
        length(test$call)
        , 1)
    expect_equal(
        unique(unlist(lapply(test[[1]], lapply, class)))
        , c("glm", "lm"))
})