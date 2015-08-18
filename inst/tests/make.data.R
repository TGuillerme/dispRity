library(Claddis)
library(mulTree)
source("../../R/sanitizing.R")
source("../../R/utilities.R")
source("../../R/tree.age.R")
source("../../R/tree.age_fun.R")
source("MorphDistMatrix.verbose.R")
source("anc.state.R")
source("anc.state_fun.R")
source("anc.unc.R")
#matrix
Nexus_data<-ReadMorphNexus("2014-Beck-ProcB-matrix-morpho.nex")
Nexus_matrix<-Nexus_data$matrix
#tree
tree<-read.nexus("2014-Beck-ProcB-TEM.tre")

######################################
#Cleaning the matrices and the trees
######################################

#Removing some species
tree_tmp<-extract.clade(tree, 133)
tree_tmp<-drop.tip(tree_tmp, extract.clade(tree_tmp, 127)$tip.label)
tree_data<-drop.tip(tree_tmp, c("Erinaceus", "Ptilocercus", "Orycteropus", "Microgale"))

#Cleaning the tree and the table
tree_data<-clean.tree(tree_data, Nexus_matrix)
table<-clean.table(Nexus_matrix, tree_data)
Nexus_data$matrix<-table
tree_data<-lapply.root(tree_data)

#Removing some characters
Nexus_data$matrix<-table
to_remove<-which(apply(Nexus_data$matrix, 2, function(X) length(levels(as.factor(X)))) < 2)
to_remove<-c(to_remove, which(apply(Nexus_data$matrix, 2, function(X) length(which(is.na(X)))) > 25))
matrix_tmp<-Nexus_data$matrix[,-c(to_remove)]
matrix_tmp<-matrix_tmp[,-c(28,59,75,98,135,172,182,186,189,191,204,224,235)]
matrix_tmp<-matrix_tmp[,-c(57,239)]
Nexus_matrix<-make.nexus(matrix_tmp)
Nexus_matrix$min.vals<-as.numeric(Nexus_matrix$min.vals)
Nexus_matrix$max.vals<-as.numeric(Nexus_matrix$max.vals)

######################################
#Estimating ancestral states
######################################

anc_states<-anc.state(tree_data, Nexus_matrix, method='ML-claddis', verbose=TRUE)

####################################
#Distance matrix
####################################

#Distance matrix using tips only
matrix_tips<-Nexus_matrix
dist_tips<-MorphDistMatrix.verbose(matrix_tips, verbose=TRUE)

#Distance matrix using also nodes
matrix_nodes<-Nexus_matrix
matrix_nodes$matrix<-anc.unc(anc_states, 0.95, missing=NA)$state
dist_nodes<-MorphDistMatrix.verbose(matrix_nodes, verbose=TRUE)

####################################
#Creating the two pco's (with and without nodes)
####################################

ord_data_tips<-cmdscale(dist_tips$gower.dist.matrix, k=nrow(dist_tips$gower.dist.matrix) - 2, add=T)$points
ord_data_tipsNnodes<-cmdscale(dist_nodes$gower.dist.matrix, k=nrow(dist_nodes$gower.dist.matrix) - 2, add=T)$points

####################################
#Loading the FADLAD data
####################################

FADLAD_data<-read.csv("Beck2014_FADLAD.csv")

test_data<-list("tree_data"=tree_data, "ord_data_tips"=ord_data_tips, "ord_data_tips_nodes"=ord_data_tipsNnodes, "FADLAD_data"=FADLAD_data)

save(test_data, file="test_data.Rda")