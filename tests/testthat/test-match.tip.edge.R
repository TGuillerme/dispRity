## Test
test_that("match.tip.edge works", {

    set.seed(3) 
    tree <- rtree(20)
    trees <- list(tree, tree, tree)
    class(trees) <- "multiPhylo"
    tip_values <- sample(c("blue", "red"), 20, replace = TRUE)
    node_values <- sample(c("blue", "red"), 19, replace = TRUE)

    ## Sanitizing
    wrong_multiphylo <- list(rtree(3), rtree(4))
    class(wrong_multiphylo) <- "multiPhylo"
    error <- capture_error(match.tip.edge(tip_values, wrong_multiphylo))
    expect_equal(error[[1]], "The trees from wrong_multiphylo must have the same number of tips.")
    error <- capture_error(match.tip.edge(tip_values, rtree(5)))
    expect_equal(error[[1]], "The input vector must of the same length as the number of tips (5) or tips and nodes (9) in phylo. Or it must be a vector of node or tips IDs or names.")
    error <- capture_error(match.tip.edge(tip_values, tree, use.parsimony = "ya"))
    expect_equal(error[[1]], "use.parsimony must be of class logical.")

    ## NA replaces
    edge_colors <- match.tip.edge(tip_values, tree)
    expect_equal(edge_colors, c(NA, "red", "red", "red", NA, NA, "red", "blue", "blue", NA, NA, NA, "red", "red", "red", NA, "red", "blue", "red", "red", "red", "red", "red", NA, NA, "blue", NA, "blue", "red", NA, NA, NA, "blue", "red", NA, "red", "blue", "blue"))
    ## Specific replaces
    edge_colors <- match.tip.edge(tip_values, tree, replace.na = "grey")
    expect_equal(edge_colors, c("grey", "red", "red", "red", "grey", "grey", "red", "blue", "blue", "grey", "grey", "grey", "red", "red", "red", "grey", "red", "blue", "red", "red", "red", "red", "red", "grey", "grey", "blue", "grey", "blue", "red", "grey", "grey", "grey", "blue", "red", "grey", "red", "blue", "blue"))
    ## Parsimony = FALSE
    edge_colors <- match.tip.edge(tip_values, tree, use.parsimony = FALSE)
    expect_equal(edge_colors, c(NA, NA, "red", "red", NA, NA, "red", "blue", "blue", NA, NA, NA, NA, "red", "red", NA, "red", "blue", NA, NA, "red", "red", "red", NA, NA, "blue", NA, "blue", "red", NA, NA, NA, "blue", "red", NA, "red", "blue", "blue"))

    ## with nodes
    edge_colors <- match.tip.edge(c(tip_values, node_values), tree)
    expect_equal(edge_colors, c("red", "red", "red", "red", "blue", "blue", "red", "blue", "blue", "blue", "red", "red", "blue", "red", "red",  "blue", "red", "blue", "blue", "red", "red", "red", "red", "red", "red", "blue", "blue", "blue", "red", "red",  "blue", "red", "blue", "red", "blue", "red", "blue", "blue"))

    ## With multiple trees
    edge_colors <- match.tip.edge(tip_values, trees)
    expect_is(edge_colors, "list")
    expect_equal(length(edge_colors), 3)
    expect_equal(edge_colors[[1]], c(NA, "red", "red", "red", NA, NA, "red", "blue", "blue", NA, NA, NA, "red", "red", "red", NA, "red", "blue", "red", "red", "red", "red", "red", NA, NA, "blue", NA, "blue", "red", NA, NA, NA, "blue", "red", NA, "red", "blue", "blue"))

    ## To the root + works with tips/node labels
    data(bird.orders)
    ## Getting the bird orders starting with a "C"
    some_orders <- sort(bird.orders$tip.label)[4:9]

    ## Errors
        edges_of_interest <- match.tip.edge(vector = some_orders, phylo  = bird.orders)
    error <- capture_error(match.tip.edge(vector = some_orders, phylo  = bird.orders, to.root = "ya"))
    expect_equal(error[[1]], "to.root must be of class logical.")

    ## Get the edges linking these orders
    edges_of_interest <- match.tip.edge(vector = some_orders, phylo  = bird.orders)
    expect_equal(edges_of_interest, c(7, 6, 5, 1, 23, 21, 17, 15, 14, 12, 10, 25, 24, 27, 26, 40, 39, 38, 30, 28, 43, 41))
    ## Works the same if to.root (in this case)
    edges_of_interest <- match.tip.edge(vector = some_orders, phylo  = bird.orders, to.root = TRUE)
    expect_equal(edges_of_interest, c(7, 6, 5, 1, 23, 21, 17, 15, 14, 12, 10, 25, 24, 27, 26, 40, 39, 38, 30, 28, 43, 41))
})