time.series.continuous<-function(data, tree, time, model, FADLAD, verbose) {

    #SLICING THE TREE
    #Number of slices
    n_slices<-length(time)

    #ages of tips/nodes + FAD/LAD
    ages_tree_FAD<-tree.age(tree)
    ages_tree_LAD<-tree.age(tree)
    #Change the age if FAD or LAD are higher/lower than the age of the tip
    for(tip in 1:nrow(FADLAD)) {
        #Replace age of the tip if FAD is higher
        if(FADLAD[tip,1] > ages_tree_FAD$ages[which(as.character(ages_tree_FAD$elements) == as.character(rownames(FADLAD)[tip]))]) {
            ages_tree_FAD$ages[which(ages_tree_FAD$elements == rownames(FADLAD)[tip])]<-FADLAD[tip,1]
        }
        #Replace age of the tip if LAD is lower
        if(FADLAD[tip,2] < ages_tree_LAD$ages[which(as.character(ages_tree_LAD$elements) == as.character(rownames(FADLAD)[tip]))]) {
            ages_tree_LAD$ages[which(ages_tree_LAD$elements == rownames(FADLAD)[tip])]<-FADLAD[tip,2]
        }
    }

    #Slicing the tree
    slice_list<-NULL
    slice_list<-list()

    #verbose
    if(verbose == TRUE) {
        message("Creating ", n_slices, " time samples through the tree:",appendLF=FALSE)
    }

    for (slice in 1:n_slices) {
        #Don't slice the tree if slice=0, simply drop tips
        if(time[slice]==0) {
            #Select the tips to drop
            taxa_to_drop<-ages_tree_LAD[which(ages_tree_LAD[1:Ntip(tree),1]!=0),2]
            #drop the tips
            sub_tree<-drop.tip(tree, tip=as.character(taxa_to_drop))
        }  else {
            #subtree
            sub_tree<-slice.tree(tree, time[slice], model, FAD=ages_tree_FAD, LAD=ages_tree_LAD)
        }
        #subtaxa list
        sub_taxa<-sub_tree$tip.label
        #subpco scores
        sub_data<-data[unique(sub_taxa),]
        #storing the results
        slice_list[[slice]]<-sub_data
        #verbose
        if(verbose == TRUE) {
            message(".",appendLF=FALSE)
        }
    }

    #verbose
    if(verbose == TRUE) {
        message("Done.\n",appendLF=FALSE)
    }


    #naming the slices
    names(slice_list)<-time

    return(slice_list)
#End   
}