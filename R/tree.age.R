##########################
#Tree ages
##########################
#Extract the node and the tips ages of a chronogram
#v1.1
#Update: Syntax and typos
##########################
#SYNTAX :
#<tree> a 'phylo' object
#<scale> the scale of the tree (i.e. the age of the root)
#<type> either 'past' if the tree must be a dated tree (units = time to past; tips=0), or 'present' if the tree is an absolute dated tree (units = time to present; root=0)
##########################
#----
#guillert(at)tcd.ie 30/06/2014
#Modified from [R-sig-phylo] nodes and taxa depth II - 21/06/2011 - Paolo Piras - ppiras(at)uniroma3.it
##########################


tree.age<-function(tree, scale, type='past'){

#SANITYZING

    #tree
    check.class(tree, 'phylo', ' must be a phylo object.')

    #scale
    if(missing(scale)) {
    	#Using the tree height as scale if scale is missing
        scale=max(dist.nodes(tree)[, Ntip(tree)+1])
    }
    check.class(scale, 'numeric', ' must be a numerical value.')
    check.length(scale, '1', ' must a a single value.')

    #type
    check.class(type, 'character', ' must be \'past\' or \'present\'.')
    if(type !='past') {
        if(type !='present') {
            stop('type must be \'past\' or \'present\'.')
        }
    }

#CALCULATE THE EDGES AGE

    if(scale == 0) {
        ages.table<-tree.age_table(tree)
    } else {
        ages.table<-tree.age_scale(tree.age_table(tree), scale)
    }

    #Type
    if(type == 'past'){
        tree.height<-max(ages.table$ages)
        ages.table$ages<-round(abs(ages.table$ages-tree.height), digit=3)
    } else {
        ages.table$ages<-round(ages.table$ages, digit=7)
    }

    #Output
    #ages.table<-round(ages.table[1,], digit=3)
    return(ages.table)

    #Example
    example=FALSE
    if(example == TRUE){
        #Examples
        ##Generate a birth-death tree with fossil and living species
        library(diversitree)
        tree<-tree.bd(c(1,0.3), max.taxa=20, include.extinct=TRUE)

        ##Calculate the edges age by setting the root a 65 Mya
        Treeage(tree, scale=65)

        ##Ploting the distribution of the node ages
        hist(ages[-c(1:length(tree$tip.label)),1], xlab="Time", main="Divergence frequency per time")

        ##Calculate when the fossil went extinct
        ages.fossil<-Treeage(tree, scale=65, type='past')
        tree.fossil<-tree
        tree.fossil$tip.label[grep("ex",ages.fossil[,2])]<-paste(tree.fossil$tip.label[grep("ex",ages.fossil[,2])],round(ages.fossil[grep("ex",ages.fossil[,2]),1], digit=2), "Mya", sep=" ")
        plot(tree.fossil)

        ##Ploting the node age from root
        ages.nodes<-Treeage(tree, scale=65, type='present') #change 'present' into 'past' to plot a classical chronogram
        tree.nodes<-tree
        tree.nodes$node.label<-round(ages.nodes[-c(1:length(tree.nodes$tip.label)),1], digit=2)
        plot(tree.nodes)
        nodelabels(tree.nodes$node.label, cex=0.6)
    }
}
