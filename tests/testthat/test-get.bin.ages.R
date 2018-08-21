## TEST tree.age

context("get.bin.ages")

## Testing tree.age_table
## example
data(BeckLee_tree)
tree <- BeckLee_tree
## Test
test_that("get.bin.ages works", {

    ## Sanitising
    expect_error(get.bin.ages("tree", what = "Start", type = "Age", ICS = 2015))
    tree_tmp <- tree
    tree_tmp$root.time <- NULL
    expect_error(get.bin.ages(tree_tmp))
    expect_error(get.bin.ages(tree, what = "bla", type = "Age", ICS = 2015))
    expect_error(get.bin.ages(tree, what = "Start", type = "age", ICS = 2015))
    expect_error(get.bin.ages(tree, what = "Start", type = "Age", ICS = 1989))

    ## Right results
    ages <- get.bin.ages(tree, what = "Start", type = "Age", ICS = 2015)
    expect_is(ages, "numeric")
    expect_equal(length(ages), 33)
    expect_equal(ages, c(139.8000, 132.9000, 129.4000, 125.0000, 113.0000, 100.5000, 93.9000, 89.8000, 86.3000, 83.6000, 72.1000, 66.0000, 61.6000, 59.2000, 56.0000, 47.8000, 41.2000, 37.8000, 33.9000, 28.1000, 23.0300, 20.4400, 15.9700, 13.8200, 11.6300, 7.2460, 5.3330, 3.6000, 2.5800, 1.8000, 0.7810, 0.1260, 0.0117))

    ## Tree without living taxa
    tree_age <- tree.age(tree)
    tree <- drop.tip(tree, tip = as.character(tree_age[,2][which(tree_age[,1] == 0)]))
    ages <- get.bin.ages(tree, what = "Start", type = "Age", ICS = 2015)
    expect_equal(ages, c(139.8000, 132.9000, 129.4000, 125.0000, 113.0000, 100.5000, 93.9000, 89.8000, 86.3000, 83.6000, 72.1000, 66.0000, 61.6000, 59.2000, 56.0000, 47.8000, 41.2000, 37.8000))
})

