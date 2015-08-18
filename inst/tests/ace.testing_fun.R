#######################################
#Testing percentage of "correct" recovery
#########################################

testing.ace<-function(n_tips, n_char, prop_na, replicates) {
    #Parameter generation function for trees
    gen.param.tree<-function(x=1) {
        lambda<-runif(x)
        mu<-runif(x,0,lambda)
        return(cbind(lambda, mu))
    }

    #Initializing
    results_list<-list("ape"=list("correct"=NULL, "error"=NULL, "na"=NULL), "ape95"=list("correct"=NULL, "error"=NULL, "na"=NULL), "claddis"=list("correct"=NULL, "error"=NULL, "na"=NULL), "claddis95"=list("correct"=NULL, "error"=NULL, "na"=NULL))

    for (rep in 1:replicates) {

        ##################################
        #Generating the tree
        ##################################

        #Birth death tree
        phy<-tree.bd(gen.param.tree(), max.taxa=n_tips)
        #make sure the tree didn't failed
        if(is.null(phy)) {
            while(is.null(phy)) {
                phy<-tree.bd(gen.param.tree(), max.taxa=n_tips)
            } 
        }
        
        ##################################
        #Generating the character states
        ##################################

        #First set of nodes and tips states
        nodes<-attr(tips<-sim.character(phy, rep(runif(1, 0, 0.5),2), x0=0, model="mk2"), which="node.state")
        #Store the results in two different tables
        #Nodes
        nodes_table<-matrix(ncol=1, data=nodes)
        #add rownames
        rownames(nodes_table)<-names(nodes)
        #Tips
        tips_table<-matrix(ncol=1, data=tips)
        #Add rownames
        rownames(tips_table)<-names(tips)
        #Fill the table with the same number of characters as the number of species
        for (character in 2:n_char) {
            #generating the states
            nodes<-attr(tips<-sim.character(phy, rep(runif(1, 0, 0.5),2), x0=0, model="mk2"), which="node.state")
            #binding the results
            nodes_table<-cbind(nodes_table, nodes) ; tips_table<-cbind(tips_table, tips)
        }
        #Removing the column names
        colnames(nodes_table)<-NULL ; colnames(tips_table)<-NULL

        ##################################
        #Removing data
        ##################################

        #Randomly removing n data (replace by NA)
        tips_table[sample(1:length(as.vector(tips_table)), length(as.vector(tips_table))*prop_na)]<-NA

        ##################################
        #Transforming into nexus data
        ##################################

        #Creating the nexus data table (as in ReadMorphNexus output)
        table.nex<-list()
        #header
        table.nex$header<-"test"
        #matrix
        table.nex$matrix<-tips_table
        #ordering
        table.nex$ordering<-rep("unord", n_char) #unordered
        #weighting
        table.nex$weights<-rep(1, n_char) #none
        #max values
        table.nex$max.vals<-apply(tips_table, 2, max, na.rm=TRUE)
        #min values
        table.nex$min.vals<-apply(tips_table, 2, min, na.rm=TRUE)
        #step.matrices
        table.nex$step.matrices<-NULL
        #symbols
        table.nex$symbols<-c("0","1")

        ##################################
        #Ancestral states reconstruction
        ##################################

        #ML-ape
        options(warn=-1)
        test_ape<-anc.state(phy, table.nex, method='ML-ape', verbose=TRUE)
        options(warn=0)
        #nodes table
        nodes_ape<-test_ape$state[-c(1:Ntip(phy)),]
        #replace ?s by NAs
        nodes_ape[grep("\\?", nodes_ape)]<-NA
        #as numeric
        options(warn=-1)
        nodes_ape<-apply(nodes_ape, 2, as.numeric)
        options(warn=0)

        #Conservative version
        test_ape95<-anc.unc(test_ape, 0.95)
        #nodes table
        nodes_ape95<-test_ape95$state[-c(1:Ntip(phy)),]
        #replace ?s by NAs
        nodes_ape95[grep("\\?", nodes_ape95)]<-NA
        #as numeric
        options(warn=-1)
        nodes_ape95<-apply(nodes_ape95, 2, as.numeric)
        options(warn=0)

        #ML-claddis
        options(warn=-1)
        test_claddis<-anc.state(phy, table.nex, method='ML-claddis', verbose=TRUE)
        options(warn=0)
        #nodes table
        nodes_claddis<-test_claddis$state[-c(1:Ntip(phy)),]
        #as numeric
        options(warn=-1)
        nodes_claddis<-apply(nodes_claddis, 2, as.numeric)
        options(warn=0)

        #Conservative version
        test_claddis95<-anc.unc(test_claddis, 0.95)
        #nodes table
        nodes_claddis95<-test_claddis95$state[-c(1:Ntip(phy)),]
        #as numeric
        options(warn=-1)
        nodes_claddis95<-apply(nodes_claddis95, 2, as.numeric)
        options(warn=0)

        #Counting NAs and errors
        results_list$ape$correct[rep]<-length(which((nodes_ape ==  nodes_table) == TRUE))/length(as.vector(nodes_table))
        results_list$ape$error[rep]<-length(which((nodes_ape ==  nodes_table) == FALSE))/length(as.vector(nodes_table))
        results_list$ape$na[rep]<-length(which(is.na(nodes_ape ==  nodes_table)))/length(as.vector(nodes_table))

        results_list$ape95$correct[rep]<-length(which((nodes_ape95 ==  nodes_table) == TRUE))/length(as.vector(nodes_table))
        results_list$ape95$error[rep]<-length(which((nodes_ape95 ==  nodes_table) == FALSE))/length(as.vector(nodes_table))
        results_list$ape95$na[rep]<-length(which(is.na(nodes_ape95 ==  nodes_table)))/length(as.vector(nodes_table))

        results_list$claddis$correct[rep]<-length(which((nodes_claddis ==  nodes_table) == TRUE))/length(as.vector(nodes_table))
        results_list$claddis$error[rep]<-length(which((nodes_claddis ==  nodes_table) == FALSE))/length(as.vector(nodes_table))
        results_list$claddis$na[rep]<-length(which(is.na(nodes_claddis ==  nodes_table)))/length(as.vector(nodes_table))

        results_list$claddis95$correct[rep]<-length(which((nodes_claddis95 ==  nodes_table) == TRUE))/length(as.vector(nodes_table))
        results_list$claddis95$error[rep]<-length(which((nodes_claddis95 ==  nodes_table) == FALSE))/length(as.vector(nodes_table))
        results_list$claddis95$na[rep]<-length(which(is.na(nodes_claddis95 ==  nodes_table)))/length(as.vector(nodes_table))
    }
return(results_list)
}