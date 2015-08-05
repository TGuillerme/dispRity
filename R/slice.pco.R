##########################
#slice.pco
##########################
#Select the number of taxa and pco data per slice.
#v0.2.1
##########################
#SYNTAX :
#<pco_data> the pco data to split in bins.
#<tree> a 'phylo' object. The tree must be dated.
#<slices> slices ages. Can be either a single value (for a single slice) or a series of values (for multiple slices)
#<method> the slicing method (what becomes of the sliced branches): can be 'random' (default), 'acctran', 'deltran' or 'proximity'.
#<FAD_LAD> a data.frame containing the first and last apparition datums. If none is provided, or if taxa are missing, taxa are assumed to have the same FAD and LAD.
#<verbose> whether to be verbose or not.
#<diversity> logical, whether to count the number of taxa in each slice.
##########################
#Method details: the slicing methods are the method of the edge to chose when cutting through a branch. At any point of the branch cut, the different method picks either the data of the parent node or one of the offspring node or tip.
#random: randomly chose between parent and offspring (default);
#acctran: always chose offspring;
#deltran: always chose parent;
#prozimity: chose between the parent or the offspring based on branch length. If the cut is equal to more than half the branch length, the offspring is chosen, else the parent.
#----
#guillert(at)tcd.ie 08/04/2015
##########################

slice.pco<-function(pco_data, tree, slices, method="random", FAD_LAD, verbose=FALSE, diversity=FALSE) {

    #SANITIZING

    #pco
    check.class(pco_data, 'matrix', ' must be a pco scores matrix.')

    #tree
    check.class(tree, 'phylo', ' must be a phylo object.')
    #the tree must be dated
    if(length(tree$root.time)==0){
        stop("Tree must be a dated tree with $root.time.")
    }
    #the tree must have node labels
    if(is.null(tree$node.label)) {
        stop('The tree must have node label names.')
    }

    #slices
    if(class(slices) != 'numeric') {
        if(class(slices) != 'integer') {
            stop("slices must be numeric.")
        }
    }
    #slice cannot be older than the root age
    if(any(slices >= tree$root.time)) {
        stop("Slices cannot be older or equal to the tree's root age.")
    }

    #method
    check.class(method, 'character', " must be either \'random\', \'acctran\', \'deltran\' or \'proximity\'.")
    check.length(method, 1, " must be either \'random\', \'acctran\', \'deltran\' or \'proximity\'.", errorif=FALSE)
    METHODS<-c("random", "acctran", "deltran", "proximity")
    if(!any(method == METHODS)) {
        stop("method must be either \'random\', \'acctran\', \'deltran\' or \'proximity\'.")
    }

    #FAD_LAD
    if(missing(FAD_LAD)) {
        #Create the FAD_LAD table
        FAD_LAD<-data.frame("FAD"=tree.age(tree)[1:Ntip(tree),1], "LAD"=tree.age(tree)[1:Ntip(tree),1], row.names=tree.age(tree)[1:Ntip(tree),2])
        message("No FAD_LAD table has been provided so every tip is assumed to interval single points in time.")
    } else {
        #Check if the FAD_LAD contains all taxa
        if(any(tree$tip.label %in% rownames(FAD_LAD) == FALSE)) {
            message("Some tips have FAD/LAD and are assumed to interval single points in time.")
            #If not generate the FAD_LAD for the missing taxa
            missing_FADLAD<-which(is.na(match(tree$tip.label, rownames(FAD_LAD))))
            add_FAD_LAD<-data.frame(tree.age(tree)[missing_FADLAD,1], tree.age(tree)[missing_FADLAD,1], row.names=tree.age(tree)[missing_FADLAD,2])
            colnames(add_FAD_LAD)<-colnames(FAD_LAD)
            FAD_LAD<-rbind(FAD_LAD, add_FAD_LAD)
        }
        #Remove FAD_LAD taxa not present in the tree
        if(nrow(FAD_LAD) != Ntip(tree)) {
            FAD_LAD<-FAD_LAD[-c(which(is.na(match(rownames(FAD_LAD), tree$tip.label)))),]
        }
    }

    #verbose
    check.class(verbose, 'logical')

    #diversity
    check.class(diversity, 'logical')

    #SLICING THE TREE
    #Number of slices
    n_slices<-length(slices)

    #ages of tips/nodes + FAD/LAD
    ages_tree_FAD<-tree.age(tree)
    ages_tree_LAD<-tree.age(tree)
    #Change the age if FAD or LAD are higher/lower than the age of the tip
    for(tip in 1:nrow(FAD_LAD)) {
        #Replace age of the tip if FAD is higher
        if(FAD_LAD[tip,1] > ages_tree_FAD$ages[which(ages_tree_FAD$edges == rownames(FAD_LAD)[tip])]) {
            ages_tree_FAD$ages[which(ages_tree_FAD$edges == rownames(FAD_LAD)[tip])]<-FAD_LAD[tip,1]
        }
        #Replace age of the tip if LAD is lower
        if(FAD_LAD[tip,2] < ages_tree_LAD$ages[which(ages_tree_LAD$edges == rownames(FAD_LAD)[tip])]) {
            ages_tree_LAD$ages[which(ages_tree_LAD$edges == rownames(FAD_LAD)[tip])]<-FAD_LAD[tip,2]
        }
    }

    #Slicing the tree
    slice_list<-NULL
    slice_list<-list()

    #verbose
    if(verbose == TRUE) {
        message("Creating ", n_slices, " slices through the tree:",appendLF=FALSE)
    }

    for (slice in 1:n_slices) {
        #Don't slice the tree if slice=0, simply drop tips
        if(slices[slice]==0) {
            #Select the tips to drop
            taxa_to_drop<-ages_tree_LAD[which(ages_tree_LAD[1:Ntip(tree),1]!=0),2]
            #drop the tips
            sub_tree<-drop.tip(tree, tip=as.character(taxa_to_drop))
        }  else {
            #subtree
            sub_tree<-slice.tree(tree, slices[slice], method, FAD=ages_tree_FAD, LAD=ages_tree_LAD)
        }
        #subtaxa list
        sub_taxa<-sub_tree$tip.label
        #subpco scores
        sub_pco<-pco_data[sub_taxa,]
        #storing the results
        slice_list[[slice]]<-sub_pco
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
    names(slice_list)<-slices

    #Diversity
    if(diversity == TRUE) {
        #count the elements per intervals
        diversity_counts<-unlist(lapply(slice_list, nrow))
        #Output
        output<-list("pco_slices"=slice_list, "diversity"=diversity_counts)
        return(output)
    
    } else {
        return(slice_list)
    }

    return(slice_list)
#End   
}