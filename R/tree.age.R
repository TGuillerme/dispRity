
#Modified from [R-sig-phylo] nodes and taxa depth II - 21/06/2011 - Paolo Piras - ppiras(at)uniroma3.it

tree.age<-function(tree, age, order='past'){

#SANITYZING

    #tree
    check.class(tree, 'phylo', ' must be a phylo object.')

    #age
    if(missing(age)) {
    	#Using the tree height as age if age is missing
        age=max(dist.nodes(tree)[, Ntip(tree)+1])
    }
    check.class(age, 'numeric', ' must be a numerical value.')
    check.length(age, '1', ' must a a single value.')

    #order
    check.class(order, 'character', ' must be \'past\' or \'present\'.')
    if(order !='past') {
        if(order !='present') {
            stop('order must be \'past\' or \'present\'.')
        }
    }

#CALCULATE THE EDGES AGE

    if(age == 0) {
        ages.table<-tree.age_table(tree)
    } else {
        ages.table<-tree.age_age(tree.age_table(tree), age)
    }

    #Type
    if(order == 'past'){
        tree.height<-max(ages.table$ages)
        ages.table$ages<-round(abs(ages.table$ages-tree.height), digit=3)
    } else {
        ages.table$ages<-round(ages.table$ages, digit=7)
    }

    #Output
    #ages.table<-round(ages.table[1,], digit=3)
    return(ages.table)
}
