###########################
#A series of function generating null model data
##########################
#Generates null trees or/and null matrices under various Brownian-like models
#v0.1
##########################
#SYNTAX :
#<tree> can be either a given phylogenetic tree or one of the following: "yule" or "bd" (see details). If "bd", an optional argument tree.par.fun can be given.
#<matrix> can be either a cladistic matrix or a single value as the number of characters in the matrix or a vector of characters states. If matrix argument is a single value or a vector, the optional argument matrix.model is needed.
#<include.nodes> logical, whether to include the nodes or not in the matrix
#<replicates> number of random replicates.
#<verbose> whether to be verbose or not.
#<n.tips> optional. If tree is not a "phylo" object and matrix is not a "matrix" object, the number of tips in the tree and in the matrix.
#<matrix.model> optional. If matrix argument is a value or a vector, can be either "random" (default) or "sim.char" (see details). If argument is "sim.char", the optional argument "max.rate" can be given as a maximum rate value.
#<max.rate> optional. If matrix model is "sim.char", must be a maximal rate value <= 1. Default = 0.5.
#<tre.par.fun> optional. A function for generating the "birth-death" tree parameters (if missing, the parameters are randomly generate with birth parameter > death parameter).
#<root.time> optional. A value for the root age of the trees (is ignored if tree is class "phylo").
##########################
#Details:
#tree="yule" will generate yule trees using the ape::rtree function
#tree="bd" will generate a birth-death tree using the diversitree::tree.bd function
#matrix.model="random" will generate random character states for each tip in the tree
#matrix.model="sim.char" will generate random characters states for each tip in the tree based on the tree structure using diversitree::sim.character function
#----
#guillert(at)tcd.ie 18/03/2015
##########################

null.data<-function(tree, matrix, include.nodes, replicates=100, verbose=TRUE, n.tips, tre.par.fun, matrix.model, max.rate, root.time=1) {
    #SANITIZING
    #tree
    if(class(tree) != "phylo") {
        build_tree<-TRUE
        if(class(tree) == "character") {
            if(tree != "yule") {
                if(tree != "bd") {
                    stop("Tree must be either a 'phylo' object or 'yule' or 'bd'.")
                }
            }
        } else {
            stop("Tree must be either a 'phylo' object or 'yule' or 'bd'.")
        }
    } else {
        build_tree<-FALSE
    }

    #tree.par.fun
    if(missing(tre.par.fun)) {
        tre.par.fun<-gen.param.tree
    } else {
        options(warn=-1)
        if(class(tre.par.fun) != "function" & tree=="bd") {
            stop("'tre.par.fun' must be a function.")
        }
        options(warn=0)
    }

    #matrix
    if(class(matrix) == "matrix") {
        build_matrix<-FALSE
        if(build_tree == FALSE) {
            stop("Both tree and matrix are given. Nothing to simulate!")
        }
    } else {
        if(class(matrix) == 'numeric' | class(matrix) == 'integer') {
            build_matrix<-TRUE
            #Isolating the number of characters and the states
            if(length(matrix) == 1) {
                matrix_characters<-matrix
                matrix_states<-rep(2, matrix)
                message('All generated characters will be binary characters.')
            } else {
                #removing characters with less than two states
                if(any(matrix < 2)){
                    silly_characters<-which(matrix < 2)
                    matrix<-matrix[-silly_characters]
                    message(paste(length(silly_characters), "characters have less than two states and are ignored."))
                }
                matrix_characters<-length(matrix)
                matrix_states<-matrix
            }
        } else {
            stop("matrix must be either a 'matrix', a numerical value or a integer vector.")
        }
    }

    #include nodes
    check.class(include.nodes, "logical")

    #matrix.model
    if(missing(matrix.model)) {
        matrix.model<-"random"
    }
    check.class(matrix.model, "character", " must be either 'random' or 'sim.char'.")
    check.length(matrix.model, 1, " must be either 'random' or 'sim.char'.", errorif=FALSE)
    if(matrix.model != "random") {
        if(matrix.model != "sim.char") {
            stop("matrix.model must be either 'random' or 'sim.char'.")
        }
    }

    #max.rate
    if(missing(max.rate)) {
        max.rate<-0.5
    }
    check.class(max.rate, "numeric", " must be a numerical value between 0 and 1.")
    check.length(max.rate, 1, " must be a numerical value between 0 and 1.", errorif=FALSE)
    if(max.rate < 0 | max.rate > 1) {
        stop("max.rate must be a numerical value between 0 and 1.")
    }
    if(max.rate == 0) {
        stop("max.rate cannot be 0!")
    }
    if(max.rate > 0.5) {
        message("max.rate is higher than 0.5: this can slow down the function.")
    }

    #n.tips
    if(missing(n.tips)) {
        if(build_tree == FALSE) {
            n.tips<-Ntip(tree)
        } else {
            if(build_matrix == FALSE) {
                n.tips<-nrow(matrix)
            } else {
                stop("n.tips is missing with no provided matrix and tree.")
            }
        }
    } else {
        if(build_tree == TRUE) {
            n.tips<-Ntip(tree)
        }
    }

    #replicates
    check.class(replicates, 'numeric', " must be numeric.")
    #make sure replicates is an entire number
    replicates<-round(replicates)

    #verbose
    check.class(verbose, 'logical', " must be logical.")

    #root.time
    check.class(root.time, 'numeric', " must be a single numeric value.")
    check.length(root.time, 1, " must be a single numeric value.", errorif=FALSE)

    #CREATING THE NULL MODELS DATA

    ###################################
    #Building the random trees
    ###################################

    if(build_tree == TRUE) {
        #be verbose
        if(verbose == TRUE) {
            message(paste("Generating", replicates, "trees:"), appendLF=FALSE)
        }
        if(tree == "yule") {
            #Yule trees
            #be verbose
            if(verbose == TRUE) {
                message(paste(rep(".",replicates), sep=""), appendLF=FALSE)
            }
            rand_trees<-rmtree(replicates, n.tips) #Change random value?
            #Adding root time + node labels
            rand_trees<-lapply(rand_trees, lapply.root, root=root.time)
            #Reseting the class to be 'multiPhylo'
            class(rand_trees)<-class(rmtree(2,3))
        } else {
            #Birth death trees
            #Empty list
            rand_trees<-list()
            #Looping viable birth death trees
            for(rep in 1:replicates) {
                phy<-tree.bd(tre.par.fun(), max.taxa=n.tips)
                #make sure the tree didn't failed
                if(is.null(phy)) {
                    while(is.null(phy)) {
                        phy<-tree.bd(tre.par.fun(), max.taxa=n.tips)
                    } 
                }
                #save the tree
                rand_trees[[rep]]<-phy
                #be verbose
                if(verbose == TRUE) {
                    message(".", appendLF=FALSE)
                }
            }
            #Adding root time + node labels
            rand_trees<-lapply(rand_trees, lapply.root, root=root.time)
            #Reseting the class to be 'multiPhylo'
            class(rand_trees)<-class(rmtree(2,3))
        }
        #be verbose
        if(verbose == TRUE) {
            message("Done.\n", appendLF=FALSE)
        }
    }

    ###################################
    #Building the random matrix
    ###################################

    if(build_matrix == TRUE) {
        #be verbose
        if(verbose == TRUE) {
            message(paste("Generating", replicates, "matrices:"), appendLF=FALSE)
        }
        if(matrix.model == "random") {
            #Random matrices
            #Empty list
            rand_matrices<-list()
            #Looping the matrices
            for (rep in 1:replicates) {
                #Generating the random states
                rand_matrices[[rep]]<-random.mat(tree, characters=matrix_characters, states=matrix_states, include.nodes=include.nodes)
                #be verbose
                if(verbose == TRUE) {
                    message(".", appendLF=FALSE)
                }                
            }

        } else {
            #Simulate characters
            #Empty list
            rand_matrices<-list()

            #looping the matrices
            for (rep in 1:replicates) {
                #Selecting the tree
                if(build_tree == TRUE) {
                    phy<-rand_trees[[rep]]
                } else {
                    phy<-tree
                }
                #Generating the simulated matrix
                rand_matrices[[rep]]<-simulate.mat(tree, matrix_characters=matrix_characters, matrix_states=matrix_states, max.rate=max.rate, include.nodes=include.nodes)
                #be verbose
                if(verbose == TRUE) {
                    message(".", appendLF=FALSE)
                }
            }
        }
        
        #Renaming the column names for all the matrices
        #if(build_tree == FALSE) {
        #    #taxa names from the original tree
        #    rand_matrices<-lapply(rand_matrices, renaming.rows, names=tree$tip.label)
        #} else {
        #    #tip labels from all the generated trees
        #    for(rep in 1:replicates) {
        #        rownames(rand_matrices[[rep]])<-rand_trees[[rep]]$tip.label
        #    }
        #}
        #be verbose
        if(verbose == TRUE) {
            message("Done.\n", appendLF=FALSE)
        }

    }

    ###################################
    #Output management
    ###################################
    
    #If both the trees and the matrices are generated
    if(build_tree == TRUE & build_matrix == TRUE) {
        output<-list("trees"=rand_trees, "matrices"=rand_matrices)
    }
    #If only the trees are generated
    if(build_tree == TRUE & build_matrix == FALSE) {
        output<-rand_trees
    }
    #If only the matrices are generated
    if(build_tree == FALSE & build_matrix == TRUE) {
        output<-rand_matrices
    }

    #output
    return(output)

#END
}