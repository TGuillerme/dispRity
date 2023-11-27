#context("slide.nodes")

## Test
test_that("slide.nodes.internal works", {
    set.seed(1)
    tree <- rtree(5)

    ## Error output (null)
    expect_null(slide.nodes.internal(nodes = c(7,8), tree = tree, slide = 4, allow.negative.root = FALSE))

    ## Works with a single node
    set.seed(42)
    ## Generating a coalescent tree
    tree <- rcoal(5)
    tree_stret_down <- slide.nodes(8, tree, slide = -0.075)
    tree_stret_up <- slide.nodes(8, tree, slide = 0.075)

    expect_is(tree_stret_down, "phylo")
    expect_is(tree_stret_up, "phylo")
    expect_equal_round(tree$edge.length[3], tree_stret_down$edge.length[3]+0.075, digits = 7)
    expect_equal_round(tree$edge.length[3], tree_stret_up$    edge.length[3]-0.075, digits = 7)
    expect_equal_round(tree$edge.length[c(4,5)], tree_stret_down$edge.length[c(4,5)]-0.075, digits = 7)
    expect_equal_round(tree$edge.length[c(4,5)], tree_stret_up$edge.length[c(4,5)]+0.0751, digits = 3)

    ## All other branch lengths remain the same
    expect_equal(tree$edge.length[-c(3,4,5)], tree_stret_down$edge.length[-c(3,4,5)])
    
    ## All other branch lengths remain the same
    expect_equal(tree$edge.length[-c(3,4,5)], tree_stret_up$edge.length[-c(3,4,5)])
})

test_that("slide.nodes works", {

    set.seed(1)
    tree <- rtree(5)

    ## Sanitizing
    error <- capture_error(slide.nodes(nodes = 1, tree = tree, slide = 0.1))
    expect_equal(error[[1]], "node(s) not found in tree.")
    error <- capture_error(slide.nodes(nodes = c(7,8,10), tree = tree, slide = 0.1))
    expect_equal(error[[1]], "node(s) not found in tree.")
    warning <- capture_warning(slide.nodes(nodes = c(6,8), tree = tree, slide = 0.1))
    expect_equal(warning[[1]], "The parent of the root node (6) cannot be slid.")
    no_edge <- tree ; no_edge$edge.length <- NULL
    error <- capture_error(slide.nodes(nodes = c(7,8), tree = no_edge, slide = 0.1))
    expect_equal(error[[1]], "The tree has no edge lengths.")

    ## Negative branch lengths
    error <- capture_error(slide.nodes(nodes = c(7,8), tree = tree, slide = 4))
    expect_equal(error[[1]], "The slide value (4) produced negative branch length(s).")

    ## Works with a single node
    set.seed(42)
    ## Generating a coalescent tree
    tree <- rcoal(5)
    tree_stret_down <- slide.nodes(8, tree, slide = -0.075)
    tree_stret_up <- slide.nodes(8, tree, slide = 0.075)

    expect_is(tree_stret_down, "phylo")
    expect_is(tree_stret_up, "phylo")
    expect_equal_round(tree$edge.length[3], tree_stret_down$edge.length[3]+0.075, digits = 7)
    expect_equal_round(tree$edge.length[3], tree_stret_up$    edge.length[3]-0.075, digits = 7)
    expect_equal_round(tree$edge.length[c(4,5)], tree_stret_down$edge.length[c(4,5)]-0.075, digits = 7)
    expect_equal_round(tree$edge.length[c(4,5)], tree_stret_up$edge.length[c(4,5)]+0.0751, digits = 3)

    ## All other branch lengths remain the same
    expect_equal(tree$edge.length[-c(3,4,5)], tree_stret_down$edge.length[-c(3,4,5)])
    
    ## All other branch lengths remain the same
    expect_equal(tree$edge.length[-c(3,4,5)], tree_stret_up$edge.length[-c(3,4,5)])

    ## Works with multiple nodes
    set.seed(42)
    tree <- rtree(50)
    move_nodes <- c(99, 93, 53, 86, 58, 63, 60, 84)
    tree_slideed <- slide.nodes(move_nodes, tree, slide = 0.07)

    ## Get the branches that changed
    changed_branches <- c(which(tree$edge[,1] %in% move_nodes), which(tree$edge[,2] %in% move_nodes))
    expect_equal(tree$edge.length[-changed_branches], tree_slideed$edge.length[-changed_branches])
    expect_equal(unique(round(abs(tree$edge.length[changed_branches] - tree_slideed$edge.length[changed_branches]), 3)), 0.07)
})
