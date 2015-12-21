#Function modified from paleotree::timeSliceTree
slice.tree<-function(tree, age, model, FAD, LAD) {

    #For adding modules (i.e. models) follow the format
    # tree_slice<-timeSliceTree(tree, age, drop.extinct=TRUE, plot=FALSE)
    # for (tip in 1:Ntip(tree_slice)) {
    #   tree_sliced$tip.label[tip]<-module(tree, tips[tip], tree_slice)
    # }

    #SANITIZING
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
        tree_slice<-paleotree::timeSliceTree(tree, age, drop.extinct=TRUE, plot=FALSE)
    )

    #Error with trees with two taxa
    if(Ntip(tree_slice) < 3) {
        stop('To few taxa in the tree at age ', age, '!')
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

                #Chose the tip/node following the given model
                if(model == "punctuated") {
                    selected_model<-sample(c("deltran", "acctran"), 1)
                } else {
                    selected_model<-model
                }

                if(selected_model == "deltran") {
                    #Parent
                    tree_sliced$tip.label[tip]<-slice.tree_DELTRAN(tree, tips[tip], tree_slice)
                }

                if(selected_model == "acctran") {
                    #Offspring
                    tree_sliced$tip.label[tip]<-slice.tree_ACCTRAN(tree, tips[tip], tree_slice)
                }

                if(selected_model == "gradual") {
                    #Closest
                    tree_sliced$tip.label[tip]<-slice.tree_GRADUAL(tree, tips[tip], tree_slice)
                }              
            }
        } 
    }

    return(tree_sliced)

}