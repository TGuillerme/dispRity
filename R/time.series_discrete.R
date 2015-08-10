##########################
#int.pco
##########################
#Select the number of taxa per intervals
#v0.2.2
##########################
#SYNTAX :
#<pco_data> the pco data to split in intervals.
#<tree> a 'phylo' object. The tree must be dated.
#<intervals> a series of intervals breaks limits.
#<FAD_LAD> a data.frame containing the first and last apparition datums. If none is provided, or if taxa are missing, taxa are assumed to have the same FAD and LAD.
#<include.nodes> logical, whether to include nodes or not in the intervals. default = FALSE. If TRUE, the nodes must be the same name in the pco_data and in the tree.
#<diversity> logical, whether to count the number of taxa in each interval.
##########################
#Update: fixed FAD_LAD to me more plastic: if input FAD_LAD contains extra taxa, they are now being discarded from the analysis.
#----
#guillert(at)tcd.ie 19/03/2014
##########################

int.pco<-function(pco_data, tree, intervals, FAD_LAD, include.nodes=FALSE, diversity=FALSE) {

    #SANITIZING
    #pco
    check.class(pco_data, 'matrix', ' must be a pco scores matrix.')

    #tree
    check.class(tree, 'phylo', ' must be a phylo object.')
    #the tree must be dated
    if(length(tree$root.time)==0){
        stop("Tree must be a dated tree with $root.time.")
    }

    #intervals
    if(class(intervals) != 'numeric') {
        if(class(intervals) != 'integer') {
            stop("intervals must be numeric.")
        }
    }
    #length must be greater than one
    if(length(intervals) < 2) {
        stop("At least two breaks should be specified for the intervals.")
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

    #include.nodes
    check.class(include.nodes, 'logical', " must be logical.")
    #Check if nodes are present in the pco_data object and in the tree
    if(include.nodes == TRUE) {
        #Check if node labels are present
        if(length(tree$node.label) == 0) {
            stop("Provided tree has no nodes labels.")
        } else {
            for (node in 1:length(tree$node.label)) {
                if(length(grep(tree$node.label[node], rownames(pco_data))) == 0) {
                    stop(paste("node", tree$node.label[node], "not found."))
                }
            }
        }

        #Check if the pco_data contains more nodes
        if(nrow(pco_data) > (Ntip(tree)+Nnode(tree))) {
            message("Some rows in pco_data are not present in the tree!")
        }

    } else {
        #Check if the pco_data contains more nodes
        if(nrow(pco_data) > (Ntip(tree)+Nnode(tree))) {
            message("Some rows in pco_data are not present in the tree!")
        }
    }

    #diversity
    check.class(diversity, "logical", " must be logical.")


    #BINING THE PCO
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

    #Empty list element per interval
    int_elements<-NULL
    int_elements<-list()

    #Attribute each taxa/node to it's interval
    for (interval in 1:(length(intervals)-1)) {
        #Select the elements of one interval
        int_elements[[interval]]<-ages_tree_FAD$edges[which(ages_tree_FAD$ages >= intervals[interval+1] & ages_tree_LAD$ages <= intervals[interval])]
    }
    
    #Remove the nodes (if necessary)
    if(include.nodes==FALSE) {
        for (interval in 1:length(int_elements)) {
        #Remove nomatch with tree$tip.label
            int_elements[[interval]]<-int_elements[[interval]][match(tree$tip.label, int_elements[[interval]])[-which(is.na(match(tree$tip.label, int_elements[[interval]])))]]
        }
    }

    #Making the pco interval list
    pco_intervals<-NULL
    pco_intervals<-list()

    for (interval in 1:length(int_elements)) {
        #Matching list
        matching<-match(as.character(int_elements[[interval]]),as.character(rownames(pco_data)))
        #If only one taxa is matching, make sure it's not a vector
        if(length(matching) == 1) {
            pco_intervals[[interval]]<-matrix(data=pco_data[matching,], nrow=1)
            rownames(pco_intervals[[interval]])<-rownames(pco_data)[matching]
        } else {
            pco_intervals[[interval]]<-pco_data[matching,]
        }
    }

    #Naming the intervals
    name_list<-NULL
    for(interval in 1:length(int_elements)) {
        name_list[interval]<-paste(intervals[interval], intervals[interval+1], sep="-")
    }
    
    #If interval is empty, send warning and delete the interval
    #list of empty intervals (empty)
    empty_intervals<-NULL
    for (interval in 1:length(pco_intervals)) {
        if(length(pco_intervals[[interval]]) == 0) {
            #Remove the interval
            empty_intervals[interval]<-interval
            #Select the empty interval
            empty_interval<-paste(intervals[interval], intervals[interval+1], sep="-")
            message("The following interval is empty: ", empty_interval, ".")
        }
    }

    #If any empty intervals
    if(!is.null(empty_intervals)) {
        #NA removal from empty_intervals vector (if any)
        if(any(is.na(empty_intervals))) {
            empty_intervals<-empty_intervals[-which(is.na(empty_intervals))]
        }
        #Removing the empty intervals
        pco_intervals<-pco_intervals[c(-empty_intervals)]
        #Removing the empty intervals names
        name_list<-name_list[-empty_intervals]
    }
    
    names(pco_intervals)<-name_list

    #Diversity
    if(diversity == TRUE) {
        #count the elements per intervals
        if(!is.null(empty_intervals)) {
            diversity_counts<-unlist(lapply(int_elements[-empty_intervals], length))
        } else {
            diversity_counts<-unlist(lapply(int_elements, length))
        }
        #add the interval names
        names(diversity_counts)<-name_list

        #Output
        output<-list("pco_intervals"=pco_intervals, "diversity"=diversity_counts)
        return(output)
    
    } else {
        return(pco_intervals)
    }
}