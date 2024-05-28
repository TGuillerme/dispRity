## Base test (BAT::alpha)
test_that("standalone works (just copying BAT functions)", {
    set.seed(1)
    dummy_matrix <- matrix(rnorm(90), 10, 9, dimnames = list(letters[1:10]))
    dendro <- hclust(dist(dummy_matrix), method = "average")
    comm <- make.BAT.comm(dummy_matrix)

    ## Expected BAT tests
    BAT_test1 <- BAT::alpha(comm)
    expect_equal(BAT_test1[[1]], 10)
    BAT_test2 <- BAT::alpha(comm, tree = dendro)
    expect_equal_round(BAT_test2[[1]], 33.26329, digits = 5)

    ## Expected dispRity errors
    # wrong matrix
    error <- capture_error(BAT.metric("dummy_matrix", BAT.fun = BAT::alpha))
    expect_equal(error[[1]], "matrix must be of class matrix.")
    # wrong fun
    error <- capture_error(BAT.metric(dummy_matrix, BAT.fun = "BAT::alpha"))
    expect_equal(error[[1]], "BAT.fun must be a function or must be one of the following: alpha.")

    ## Expected dispRity tests
    expect_equal(BAT.metric(dummy_matrix, BAT.fun = BAT::alpha), 10)
    expect_equal(BAT.metric(dummy_matrix, BAT.fun = "alpha"), 10)
    expect_equal_round(BAT.metric(dummy_matrix, BAT.fun = BAT::alpha, BAT.args = list(tree = dendro)), 33.26329, digits = 5)
    expect_equal_round(BAT.metric(dummy_matrix, BAT.fun = "alpha", BAT.args = list(tree = dendro)), 33.26329, digits = 5)
})

test_that("dispRity pipeline workable", {
    data(demo_data)
    eco_data <- demo_data$jones
    
    ## BAT test
    data <- dispRity.BAT(eco_data)
    test <- BAT::alpha(data$comm)
    expect_equal(c(test), c(24, 24))

    ## Apply the alpha diversity on these subsets
    alpha_diversity <- dispRity(eco_data, metric = BAT.metric, BAT.fun = BAT::alpha)
    expect_equal(c(summary(alpha_diversity)$obs), c(24, 24))
    alpha_diversity <- dispRity(eco_data, metric = BAT.metric, BAT.fun = "alpha")
    expect_equal(c(summary(alpha_diversity)$obs), c(24, 24))
})

test_that("works for more complex ones", {

    data(demo_data)
    eco_data <- demo_data$jones
    data <- dispRity.BAT(eco_data)
    comm <- make.BAT.comm(data$traits)

    ## Trees
    tree <- hclust(dist(data$traits), method = "average") 
    tree_alpha      <- BAT::alpha(data$comm, tree)
    tree_dispersion <- BAT::dispersion(data$comm, tree)
    tree_evenness   <- BAT::evenness(data$comm, tree)


    #TG: PROBLEM HERE:
        # dispRity does: subsets then applies de metrics (so the options are applied to all)
        # BAT does: metric + option (applied to all) and then subsets.
    test <- dispRity.BAT(eco_data)
    test$tree <- hclust(dist(test$traits), method = "average")
    BAT::alpha(test$comm, tree = test$tree)
    # TG: SOLUTION HERE?
        # use dispRity.BAT to create a comm with all the bootstraps and subsets > feed it to BAT.metric() > output the results in a handalable way
    # Solution 2: make BAT run for whole data as well as subset


    # tust <- dispRity.BAT(boot.matrix(eco_data, bootstraps = 3))


    # test <- BAT.metric(data$traits, BAT.fun = alpha, BAT.args = list(tree = tree))
    # expect_equal(c(test), c(BAT::alpha(comm, tree)))
    # test <- dispRity(eco_data, metric = BAT.metric, BAT.fun = BAT::alpha, BAT.args = list(tree = tree))
    # expect_equal(c(summary(test)$obs), c(tree_alpha))

    # test <- BAT.metric(data$traits, BAT.fun = dispersion, BAT.args = list(tree = tree))
    # expect_equal(c(test), c(BAT::dispersion(comm, tree)))
    # test <- dispRity(eco_data, metric = BAT.metric, BAT.fun = dispersion, BAT.args = list(tree = tree))
    # expect_equal(c(summary(test)$obs), c(tree_dispersion))

    # test <- BAT.metric(data$traits, BAT.fun = evenness, BAT.args = list(tree = tree))
    # expect_equal(c(test), c(BAT::evenness(comm, tree)))
    # test <- dispRity(eco_data, metric = BAT.metric, BAT.fun = evenness, BAT.args = list(tree = tree))
    # expect_equal(c(summary(test)$obs), c(tree_evenness))

    # ## Kernels
    # hypervolume <- BAT::kernel.build(comm = t(presence), trait = traits))

    # richness   <- BAT::kernel.alpha(comm=hypervolume)))
    # dispersion <- BAT::kernel.dispersion(comm = hypervolume)))
    # regularity <- BAT::kernel.evenness(comm = hypervolume)))

    # ## Hulls
    # hull <-    BAT::hull.build(comm = t(presence), trait = traits))
    # results <- BAT::hull.alpha(hull))
})




# alpha <- function(comm, tree, raref = 0, runs = 100){

#   #convert traits to a tree if needed
#   if(!missing(tree) && (is.matrix(tree) || is.data.frame(tree) || is.vector(tree)))
#     tree = tree.build(tree)
  
#   #first organize the data
#   if(!missing(tree)){
#     cleanData = clean(comm, tree)
#     comm = cleanData[[1]]
#     tree = cleanData[[2]]
#   }
  
#   #now let's go for what matters
#   nComm <- nrow(comm)
#   if(raref < 1){                        # no rarefaction if 0 or negative
#     results <- matrix(0, nComm, 1)
#     for (s in 1:nComm){
#       results[s,1] <- sobs(comm[s,, drop=FALSE], tree)
#     }
#     rownames(results) <- rownames(comm)
#     colnames(results) <- "Richness"
#     return (results)
#   }
#   if (raref == 1)
#     raref <- nMin(comm)             # rarefy by minimum n among all communities
#   results <- matrix(0, nComm, 6)
#   for (s in 1:nComm){
#     res <- c()
#     for (r in 1:runs){
#       res <- c(res,sobs(rrarefy(comm[s,], raref), tree))
#     }
#     results[s,] <- c(mean(res), quantile(res, 0.5), min(res), quantile(res, 0.025), quantile(res, 0.975), max(res))
#   }
#   rownames(results) <- rownames(comm)
#   colnames(results) <- c("Mean", "Median", "Min", "LowerCL", "UpperCL", "Max")
#   return (results)
# }