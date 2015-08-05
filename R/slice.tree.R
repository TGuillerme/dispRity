##########################
#slice.tree
##########################
#Slices a tree given a specific age
#Modified from paleotree::timeSliceTree
#v1.0
#Update: added RATES method
#Update: changed the RATES method into PROXIMITY
#Update: added possibility of adding FAD_LAD data
#Update: added random method
##########################
#SYNTAX :
#<tree> a 'phylo' object
#<age> where to slice the tree
#<method> the slicing method (what becomes of the sliced branches): can be random, acctran, deltran or proximity.
#<FAD> optional First Apparition Datum data (tree.age format)
#<LAD> optional Last Apparition Datum data (tree.age format)
##########################
#Method details: the slicing methods are the method of the edge to chose when cutting through a branch. At any point of the branch cut, the different method picks either the data of the parent node or one of the offspring node or tip.
#random: randomly chose between parent and offspring (default);
#acctran: always chose offspring;
#deltran: always chose parent;
#prozimity: chose between the parent or the offspring based on branch length. If the cut is equal to more than half the branch length, the offspring is chosen, else the parent.
#----
#guillert(at)tcd.ie 10/03/2015
##########################

slice.tree<-function(tree, age, method, FAD, LAD) {

    #SANITIZING
    #tree
    check.class(tree, 'phylo')
    #must have node labels
    if(is.null(tree$node.label)) {
        stop('The tree must have node label names.')
    }

    #age
    check.class(age, 'numeric')
    #age must be at least higher than the root age
    if(age > tree$root.time) {
        stop("age cannot be older than the tree's root age.")
    }

    #method
    check.class(method, 'character', " must be either \'random\', \'acctran\', \'deltran\' or \'proximity\'.")
    check.length(method, 1, " must be either \'random\', \'acctran\', \'deltran\' or \'proximity\'.", errorif=FALSE)
    METHODS<-c("random", "acctran", "deltran", "proximity")
    if(!any(method == METHODS)) {
        stop("method must be either \'random\', \'acctran\', \'deltran\' or \'proximity\'.")
    }

    #FAD/LAD
    if(missing(FAD)) {
        FAD<-tree.age(tree)
    }
    if(missing(LAD)) {
        LAD<-tree.age(tree)
    }

    #SLICING A TREE
    #Creating the tree.age matrix
    tree_age<-tree.age(tree)

    #Running the timeSliceTree function (remove warning, called as a message in the original function)
    suppressMessages(
        tree_slice<-timeSliceTree(tree, age, drop.extinct=TRUE, plot=FALSE)
    )

    #Error with trees with two taxa
    if(Ntip(tree_slice) < 3) {
        stop('To few taxa for the tree slice at age ', age, '!')
    }

    #Selecting the tips
    tips<-tree_slice$tip.label

    #renaming the tree_slice
    tree_sliced<-tree_slice

    #Correcting the sliced tree
    for (tip in 1:Ntip(tree_slice)) {

        #Check if the tree is sliced at the exact age of a tip (e.g. time=0)
        if(tree_age[which(tree_age[,2]==tips[tip]),1] == age) {
            #Save the tip
            tree_slice$tip.label[tip]<-tree_slice$tip.label[tip]

        } else {

            #Check if the age of the tip is in between the FAD/LAD
            if(FAD[which(FAD[,2]==tips[tip]),1] >= age & LAD[which(LAD[,2]==tips[tip]),1] <= age) {
                #Save the tip
                tree_slice$tip.label[tip]<-tree_slice$tip.label[tip]

            } else {

                #Chose the tip/node following the given method
                if(method == "random") {
                    selected_method<-sample(c("deltran", "acctran"), 1)
                } else {
                    selected_method<-method
                }

                if(selected_method == "deltran") {
                    #Parent
                    tree_sliced$tip.label[tip]<-slice.tree_DELTRAN(tree, tips[tip], tree_slice)
                }

                if(selected_method == "acctran") {
                    #Offspring
                    tree_sliced$tip.label[tip]<-slice.tree_ACCTRAN(tree, tips[tip], tree_slice)
                }

                if(selected_method == "proximity") {
                    #Closest
                    tree_sliced$tip.label[tip]<-slice.tree_PROXIMITY(tree, tips[tip], tree_slice)
                }              
            }
        } 
    }

    return(tree_sliced)

}