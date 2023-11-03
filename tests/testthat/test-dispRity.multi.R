#context("dispRity.utilities")

# 1 - sanitizing, detect if the trees and matrices don't match. If they don't you need the same number of trees and matrices. Then output apply.multi.trees activator


# - test case Mario

# Email Mario:
# For cleaning the trees, you can use dispRity::clean.data
# For removing the 0 branch lengths, you can use dispRity::remove.zero.brlen (which shifts nodes to remove 0 values rather than adding branch lengths which can results in the trees not being ultrametric anymore)

matrix <- Claddis::read_nexus_matrix("~/R/data/Matrix_20230214", equalize_weights = TRUE)
tree <- read.nexus("~/R/data/sample100")
data <- Claddis::ordinate_cladistic_matrix(morpho_matrix)$vectors

## Tips in the tree and data are the same


#Extract the vector matrix from the Claddis morphospace object to feed it to dispRity
morphosp = morphospace$vectors
rownames(morphosp) = c(tree$tip.label,tree$node.label)


######POARS analysis
####################

## Time slices
time_slices <- seq(from = 0, to = 280, by = 10)

## Gradual splits model
gradual_model <- chrono.subsets(morphosp, tree, time = time_slices, method = "continuous",
                                model = "gradual.split", inc.nodes=TRUE)



# 2 - run the apply.multi.trees
# 3 - depending on the function, combine or leave the results as is.


## utilities internals
test_that("sanitizing: detects multiple matrices/trees combinations", {

    ## 5 trees and matrices
    tree <- rmtree(5, 5)
    tree <- lapply(tree, makeNodeLabel)
    class(tree) <- "multiPhylo"
    data <- matrix(1, nrow = 9, ncol = 1, dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label)))
    data <- replicate(5, data, simplify = FALSE)

    ## Modifying one tree and one matrix
    tree[[1]]$node.label[1] <- rownames(data[[1]])[6] <- "nooooode"

    ## Matrix with all nodes
    data_all <- matrix(1, nrow = 10, ncol = 1, dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label, "nooooode")))




    
    check.labels <- function(tree, data) {
        return(all(rownames(data) %in% c(tree$tip.label, tree$node.label)))
    }

    # - case 1: same number of trees and matrices but different labels in all
    length(data) == length(tree)
    test <- all(check.labels, tree, data, SIMPLIFY = FALSE)))

    # - case 2: one matrix (with all labels) but multiple trees with different labels
    length(data) == 1 && length(tree) > 1
    all(unlist(lapply(tree, check.labels, data[[1]])))
    
    # - case 3: multiple matrices (with all labels) but multiple trees
    length(data) > 1 && length(tree) > 1 && all(sort(unique(unlist(lapply(data, rownames)))) == rownames(data[[1]]))
    all(unlist(lapply(tree, check.labels, data[[1]])))
    #TODO: replicate the number of matrices per tree
    out_expected <- length(data)*length(tree)



})