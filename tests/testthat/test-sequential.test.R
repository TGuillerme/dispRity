context("sequential.test")

#Testing data
data(BeckLee_mat50)
factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)),
     dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
sum_of_variances <- dispRity(boot.matrix(cust.series(BeckLee_mat50, factors), bootstraps = 100), metric = c(sum, variances))
series <- extract.dispRity(sum_of_variances, observed = FALSE)
seq_series <- list(c(1,2), c(2,3))


test_that("set.pair.series internal", {
    set.seed(1)
    series_pair <- list(replicate(3,rnorm(10), simplify = FALSE), replicate(3,rnorm(10, 100), simplify = FALSE))
    #Errors
    expect_error(
        set.pair.series("a", NULL)
        )
    expect_error(
        set.pair.series(list(), NULL)
        )

    #Normal results
    expect_is(
        set.pair.series(series_pair, intercept = NULL)
        ,"list")
    expect_equal(
        unique(unlist(lapply(set.pair.series(series_pair, intercept = NULL), class)))
        ,"data.frame")
    expect_equal(
        unique(unlist(lapply(set.pair.series(series_pair, intercept = NULL), dim)))
        ,c(20, 2))
    expect_equal(
        unique(unlist(lapply(set.pair.series(series_pair, intercept = "a"), dim)))
        ,c(20, 3))
    expect_lt(
        max(set.pair.series(series_pair, intercept = NULL)[[1]][[1]][1:10])
        ,50)
    expect_gt(
        min(set.pair.series(series_pair, intercept = NULL)[[1]][[1]][11:20])
        ,50)
    expect_equal(
        unique(set.pair.series(series_pair, intercept = NULL)[[1]][[2]][1:10])
        ,0)
    expect_equal(
        unique(set.pair.series(series_pair, intercept = NULL)[[1]][[2]][11:20])
        ,1)
    expect_false(
        set.pair.series(series_pair, intercept = NULL)[[1]][[1]][1] == set.pair.series(series_pair, intercept = NULL)[[2]][[1]][1]
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

# test_that("set.intercept.next internal", {
#     #glm example
#     counts <- c(18,17,15,20,10,20,25,13,12) ; outcome <- gl(3,1,9) ; treatment <- gl(3,3)
#     d.AD <- data.frame(treatment, outcome, counts)
#     glm.D93 <- glm(counts ~ outcome, family = poisson())
    
#     #Errors
#     expect_error(
#         set.intercept.next(d.AD)
#         )
    
#     #Calcualte the right intercept
#     expect_equal(
#         set.intercept0(glm.D93)[2]
#         ,coef(glm.D93)[1])
#     expect_equal(
#         set.intercept.next(glm.D93)[1]
#         ,coef(glm.D93)[1] + coef(glm.D93)[2] * 1)
# })

# test_that("create.model works", {
#     #Errors
#     expect_error(
#         create.model("a", family = gaussian)
#         )
#     expect_error(
#         create.model(matrix(2,2), family = gaussian)
#         )
#     expect_error(
#         create.model(set.pair.series(series[seq_series[[1]]]), family = "whatever")
#         )
#     #Normal results
#     expect_is(
#         create.model(set.pair.series(series[seq_series[[1]]]), family = gaussian)
#         , c("glm","lm"))
#     expect_equal(
#         length(create.model(set.pair.series(series[seq_series[[1]]]), family = gaussian))
#         , 30)
# })

# test_that("save.results works", {
#     model <- create.model(set.pair.series(series[seq_series[[1]]]), family = gaussian)
#     #Errors
#     expect_true(
#         is.na(save.results(model = "model", results = "coefficients"))
#         )
#     expect_true(
#         is.null(save.results(model = model, results = "whatever")[[1]])
#         )
#     #Normal results
#     expect_is(
#         save.results(model = model, results = "coefficients")
#         , "list")
#     expect_equal(
#         length(save.results(model = model, results = "coefficients"))
#         , 1)
#     expect_equal(
#         length(save.results(model = model, results = c("coefficients", "terms", "family")))
#         , 3)
#     expect_equal(
#         names(save.results(model = model, results = c("coefficients", "terms", "family")))
#         , c("coefficients", "terms", "family"))
# })

# test_that("sequential.test works", {
#     #Errors
#     expect_error(
#         sequential.test(sum_of_variances, family = gaussian)
#         )
#     expect_error(
#         sequential.test(series, family = c(1, 2))
#         )
#     #results
#     expect_is(
#         sequential.test(series, family = gaussian)
#         , "list")
#     expect_equal(
#         length(sequential.test(series, family = gaussian))
#         , 2)
#     expect_equal(
#         names(sequential.test(series, family = gaussian))
#         , c("Intercept", "Slope"))
# })