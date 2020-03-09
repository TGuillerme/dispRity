library(dispRity)
library(paleotree)
library(geiger)
source("multi.ace.R")
source("convert.tokens.R")
source("read.nexus.data.R")

#####################################
## Model test data
#####################################

## Test data
set.seed(42)
data(BeckLee_mat99) ; data(BeckLee_ages) ; data(BeckLee_tree)
data_bootstrapped <- boot.matrix(chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", rev(seq(from = 0, to = 120, by = 5)), model = "proximity"))
model_test_data <- dispRity(data_bootstrapped, c(sum, variances))

save(model_test_data, file="../model_test_data.Rda")



#####################################
## Paleotree data
#####################################
library(paleotree)
set.seed(42)
record <- paleotree::simFossilRecord(p = 0.1, q = 0.1, nruns = 1, nTotalTaxa = c(10,15), nExtant = c(10,15))
taxa <- paleotree::fossilRecord2fossilTaxa(record)
rangesCont <- paleotree::sampleRanges(taxa, r = 0.5)
cladogram <- paleotree::taxa2cladogram(taxa, plot = FALSE)
likFun <- paleotree::make_durationFreqCont(rangesCont)
srRes <- optim(paleotree::parInit(likFun), likFun, lower = paleotree::parLower(likFun), upper = paleotree::parUpper(likFun), method = "L-BFGS-B", control = list(maxit = 1000000))
sRate <- srRes[[1]][2]
divRate <- srRes[[1]][1]
tree <- paleotree::cal3TimePaleoPhy(cladogram, rangesCont, brRate = divRate, extRate = divRate, sampRate = sRate, ntrees = 2, plot = FALSE)
tree[[1]]$node.label <- tree[[2]]$node.label <- paste0("n", 1:Nnode(tree[[1]]))
## Scale the trees to have the same most recent root age
tree[[1]]$root.time <- tree[[2]]$root.time <- tree[[2]]$root.time
## Make the dummy data
set.seed(1)
data <- matrix(rnorm((Ntip(tree[[1]])+Nnode(tree[[1]]))*6), nrow = Ntip(tree[[1]])+Nnode(tree[[1]]), ncol = 6, dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label)))

paleotree_data <- list(tree = tree, data = data)

save(paleotree_data, file="../paleotree_test_data.Rda")




####################
## geiger data
####################

geiger_test_data <- get(data(geospiza))

save(geiger_test_data, file="../geiger_test_data.Rda")
