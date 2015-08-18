time.series.discrete<-function(data, tree, time, FADLAD, inc.nodes) {
    #BINING THE DATA
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

    #Empty list element per interval
    int_elements<-NULL
    int_elements<-list()

    #Attribute each taxa/node to it's interval
    for (interval in 1:(length(time)-1)) {
        #Select the elements of one interval
        int_elements[[interval]]<-ages_tree_FAD$elements[which(ages_tree_FAD$ages >= time[interval+1] & ages_tree_LAD$ages <= time[interval])]
    }
    
    #Remove the nodes (if necessary)
    if(inc.nodes==FALSE) {
        for (interval in 1:length(int_elements)) {
        #Remove nomatch with tree$tip.label
            int_elements[[interval]]<-int_elements[[interval]][match(tree$tip.label, int_elements[[interval]])[-which(is.na(match(tree$tip.label, int_elements[[interval]])))]]
        }
    }

    #Making the pco interval list
    time_series<-NULL
    time_series<-list()

    for (interval in 1:length(int_elements)) {
        #Matching list
        matching<-match(as.character(int_elements[[interval]]),as.character(rownames(data)))
        #If only one taxa is matching, make sure it's not a vector
        if(length(matching) == 1) {
            time_series[[interval]]<-matrix(data=data[matching,], nrow=1)
            rownames(time_series[[interval]])<-rownames(data)[matching]
        } else {
            time_series[[interval]]<-data[matching,]
        }
    }

    #Naming the series
    name_list<-NULL
    for(interval in 1:length(int_elements)) {
        name_list[interval]<-paste(time[interval], time[interval+1], sep="-")
    }
    
    #If interval is empty, send warning and delete the interval
    #list of empty series 
    empty_time<-NULL
    for (interval in 1:length(time_series)) {
        if(length(time_series[[interval]]) == 0) {
            #Remove the interval
            empty_time[interval]<-interval
            #Select the empty interval
            empty_interval<-paste(time[interval], time[interval+1], sep="-")
            message("The following interval is empty: ", empty_interval, ".")
        }
    }

    return(time_series)
}