# ###########################
# #Utility functions
# ##########################

# ##########################
# #make.nexus
# ##########################
# #Generates a list following Claddis::ReadMorphNexus format
# #----
# #SYNTAX :
# #<matrix> a character matrix
# #<header> optional. a header name
# #<ordering> optional. a vector of "unord" / "ord" of the length of the matrix columns
# #<weights> optional. a numeric vector of the length of the matrix columns
# #----
# make.nexus<-function(matrix, header, ordering, weights) {
#     #SANITIZING
#     #matrix
#     check.class(matrix, "matrix")

#     #header
#     if(missing(header)) {
#         header<-NA
#     } else {
#         check.class(header, "character")
#         check.length(header, 1, " must be a single character string.", errorif=FALSE)
#     }

#     #ordering
#     if(missing(ordering)) {
#         ordering<-rep("unord", ncol(matrix))
#     } else {
#         check.class(ordering, "character")
#         check.length(ordering, ncol(matrix), " must be the same length as the matrix.", errorif=FALSE)
#         options(warn=-1)
#         if(any(ordering != c("unord", "ord"))) {
#             stop("Ordering vector must contain only 'unord' or/and 'ord' values.")
#         }
#         options(warn=0)
#     }

#     #weights
#     if(missing(weights)) {
#         weights<-rep(1, ncol(matrix))
#     } else {
#         check.class(weights, "integer")
#         check.length(weights, ncol(matrix), " must be the same length as the matrix.", errorif=FALSE)
#     }

#     #BUILD THE NEXUS OBJECT
#     nexus<-list()
#     nexus$header<-header
#     nexus$matrix<-matrix
#     nexus$ordering<-ordering
#     nexus$weights<-weights
#     nexus$max.vals<-apply(matrix, 2, max, na.rm=TRUE)
#     nexus$min.vals<-apply(matrix, 2, min, na.rm=TRUE)

#     return(nexus)
# }

# ##########################
# #cor.diversity
# ##########################
# #Correct a diversity vector
# #----
# #SYNTAX :
# #<diversity> a vector of taxa count as produced by the disparity function
# #<minimum> the minimum number of species per time intervals
# #----
# cor.diversity<-function(diversity, minimum=3) {
#     #SANITIZING
#     check.class(diversity, "integer", " must be a vector of taxa counts.")
#     if(is.null(names(diversity))) {
#         stop("diversity must be a vector of taxa counts with intervals names.")
#     }
    
#     #CORRECTING THE DIVERSITY VECTOR
#     while(any(diversity < minimum)) {
        
#         #Selecting the wrong interval
#         wrong_intervals<-which(diversity < minimum)
#         names(wrong_intervals)<-NULL
        
#         #Moving the first wrong interval to the next interval in time (unless the wrong interval is the last one)
#         if(wrong_intervals[1] != length(diversity)) {
#             host_interval<-wrong_intervals[1]+1
#             names(host_interval)<-NULL
#         } else {
#             #Moving the wrong interval in the preceding one
#             host_interval<-wrong_intervals[1]-1
#             names(host_interval)<-NULL
#         }

#         #Creating the new interval
#         new_interval<-diversity[wrong_intervals[1]]+diversity[host_interval] ; names(new_interval)<-NULL
#         #Creating the new diversity data
#         new_diversity<-diversity ; names(new_diversity)<-names(diversity)
#         #replacing the wrong interval
#         new_diversity[host_interval]<-new_interval
#         #renaming the interval
#         if(wrong_intervals[1] != length(diversity)) {
#             names(new_diversity)[host_interval]<-paste(strsplit(names(new_diversity)[wrong_intervals[1]], split="-")[[1]][1],strsplit(names(new_diversity)[host_interval], split="-")[[1]][2],sep="-")
#         } else {
#             names(new_diversity)[host_interval]<-paste(strsplit(names(new_diversity)[host_interval], split="-")[[1]][1],strsplit(names(new_diversity)[wrong_intervals[1]], split="-")[[1]][2],sep="-")
#         }
#         #removing empty interval
#         new_diversity<-new_diversity[-wrong_intervals[1]]
#         diversity<-new_diversity
#     }

#     return(diversity)
# }


# ##########################
# #cor.diversity
# ##########################
# #Correct a time_pco list to contain a given minimum of species
# #----
# #SYNTAX :
# #<time_pco> a vector of time_pco data
# #<minimum> the minimum number of species per time intervals
# #----
# cor.time.pco<-function(time_pco, minimum=3) {

#     while(any(as.numeric(unlist(lapply(time_pco, nrow))) < minimum)) {
#         wrong_intervals<-which(as.numeric(unlist(lapply(time_pco, nrow))) < minimum)
#         #Moving the first wrong interval to the next interval in time (unless the wrong interval is the last one)
#         if(wrong_intervals[1] != length(time_pco)) {
#             host_interval<-wrong_intervals[1]+1
#             message("Intervals ", names(time_pco)[wrong_intervals[1]], " and ", names(time_pco)[host_interval], " are combined due to insufficient data.")
#         } else {
#             #Moving the wrong interval in the preceding one
#             host_interval<-wrong_intervals[1]-1
#             message("Intervals ", names(time_pco)[host_interval], " and ", names(time_pco)[wrong_intervals[1]], " are combined due to insufficient data.")
#         }

#         #If both the host and the wrong interval have the same rownames, just delete the wrong interval and rename the host interval
#         options(warn=-1) #warn off
#         if(nrow(time_pco[[wrong_intervals[1]]]) == nrow(time_pco[[host_interval]]) &
#             all(sort(rownames(time_pco[[wrong_intervals[1]]])) == sort(rownames(time_pco[[host_interval]])))) {
         
#             #Creating the new time_pco data
#             new_time_pco<-time_pco
#             #renaming the interval
#             if(wrong_intervals[1] != length(time_pco)) {
#                 names(new_time_pco)[host_interval]<-paste(strsplit(names(new_time_pco)[wrong_intervals[1]], split="-")[[1]][1],strsplit(names(new_time_pco)[host_interval], split="-")[[1]][2],sep="-")
#             } else {
#                 names(new_time_pco)[host_interval]<-paste(strsplit(names(new_time_pco)[host_interval], split="-")[[1]][1],strsplit(names(new_time_pco)[wrong_intervals[1]], split="-")[[1]][2],sep="-")
#             }
#             #Removing the wrong time interval
#             new_time_pco[[wrong_intervals[1]]]<-NULL

#         } else {

#             #Creating the new interval
#             new_interval<-rbind(time_pco[[wrong_intervals[1]]], time_pco[[host_interval]])
#             #Making sure there are no duplicated taxa in the new interval
#             new_interval<-new_interval[c(unique(rownames(new_interval))),]
#             #Creating the new time_pco data
#             new_time_pco<-time_pco ; names(new_time_pco)<-names(time_pco)
#             #replacing the wrong interval
#             new_time_pco[[host_interval]]<-new_interval
#             #renaming the interval
#             if(wrong_intervals[1] != length(time_pco)) {
#                 names(new_time_pco)[host_interval]<-paste(strsplit(names(new_time_pco)[wrong_intervals[1]], split="-")[[1]][1],strsplit(names(new_time_pco)[host_interval], split="-")[[1]][2],sep="-")
#             } else {
#                 names(new_time_pco)[host_interval]<-paste(strsplit(names(new_time_pco)[host_interval], split="-")[[1]][1],strsplit(names(new_time_pco)[wrong_intervals[1]], split="-")[[1]][2],sep="-")
#             }
#             #removing empty interval
#             new_time_pco[[wrong_intervals[1]]]<-NULL

#         }
#         options(warn=0) #warn on
#         time_pco<-new_time_pco
#     }

#     return(time_pco)
# }

# ##########################
# #states.count
# ##########################
# #Count the number of states per characters
# #----
# #SYNTAX :
# #<character> a vector of character states
# #----
# states.count<-function(character) {
#     #Isolate the states
#     states<-levels(as.factor(character))
#     #Check if multi states
#     if(length(grep("&", states)) > 0) {
#         #Isolating the multi states
#         multi_states<-states[grep("&", states)]
#         multi_states<-as.factor(unlist(strsplit(multi_states, split="&")))
#         #Removing the multi states from the states list
#         states<-states[-grep("&", states)]
#         #Check if any of the multi states is not yet present in the states list
#         if(any(is.na(match(levels(multi_states), states)))) {
#             states<-c(states, multi_states[which(is.na(match(levels(multi_states), states)))])
#         }
#     }
#     #Count the number of states
#     return(length(states))
# }

# ##########################
# #extract.dist
# ##########################
# #extract a distance from a distance matrix build using Claddis::MorphDistMatrix
# #----
# #SYNTAX :
# #<dist.list> the distances list output from Claddis::MorphDistMatrix
# #<distance> the selected distance
# #----
# extract.dist<-function(dist.list, distance) {
#     #SANITIZING
#     #dist.list
#     check.class(dist.list, "list")

#     #Extracting the distance element from the distance list
#     if(distance == "numeric") {
#         output<-dist.list[[distance]]
#     } else {
#         output<-dist.list[[grep(distance, names(dist.list))]]
#     }
#     return(output)
# }

# ##########################
# #combine.disp
# ##########################
# #combine disparity lists generated by time.disparity
# #----
# #SYNTAX :
# #<disp.list> the disparity list
# #----
# combine.disp<-function(disp.list) {
#     #SANITIZING
#     check.class(disp.list, "list")

#     #Checking if values are saved or not
#     if(class(disp.list[[1]]) == 'data.frame') {
#         save.all<-FALSE
#     } else {
#         save.all<-TRUE
#     }

#     #merging the data
#     if(save.all==FALSE) {
#         merging<-disp.list[[1]]
#         #selecting only the numeric columns
#         select_col<-NA
#         for(col in 1:ncol(merging)) {
#             select_col[col]<-class(merging[,col])
#         }
#         select_col<-which(select_col == 'numeric')
#         #adding the other elements of disp.list
#         for(element in 2:length(disp.list)) {
#             merging[,select_col]<-merging[,select_col] + disp.list[[element]][,select_col]
#         }
#         #averaging
#         merging[,select_col]<-merging[,select_col]/length(disp.list)
#         output<-merging
    
#     } else {
#         #separating table and values
#         merging_table<-disp.list[[1]][[1]]
#         merging_value<-disp.list[[1]][[2]]
#         #selecting only the numeric columns
#         select_col<-NA
#         for(col in 1:ncol(merging_table)) {
#             select_col[col]<-class(merging_table[,col])
#         }
#         select_col<-which(select_col == 'numeric')
#         #adding the other elements of disp.list
#         for(element in 2:length(disp.list)) {
#             merging_table[,select_col]<-merging_table[,select_col] + disp.list[[element]][[1]][,select_col]
#             for(interval in 1:length(merging_value)) {
#                 merging_value[[interval]]<-c(merging_value[[interval]],disp.list[[element]][[2]][[interval]])
#             }
#         }
#         #averaging
#         merging_table[,select_col]<-merging_table[,select_col]/length(disp.list)
#         merging_value<-lapply(merging_value, as.vector)
#         output<-list("quantiles"=merging_table, "values"=merging_value)
#     }

#     return(output)
# }


# ##########################
# #lapply.root
# ##########################
# #Adding a root time and node labels to a tree (for lapply loops)
# #----
# #SYNTAX :
# #<tree> input tree
# #<root> a root age (if default, root age is automatically calculated using tree.age function)
# #<node> a node prefix (default = "n")
# #----
# lapply.root<-function(tree, root, prefix="n") {
    
#     #calculate the root (optional)
#     if(missing(root)) {
#         root<-max(tree.age(tree)$ages)
#     }

#     #add the root
#     tree$root.time<-root

#     #add the node labels
#     tree$node.label<-paste(prefix,seq(1:Nnode(tree)), sep="")
#     return(tree)
# }

# ##########################
# #extract.disp
# ##########################
# #extract a series of disparity measurement using a number of taxa (can be max or min) 
# #----
# #SYNTAX :
# #<disp.data> a disparity data.frame with a "time" and a "rarefaction" column name
# #<rarefaction> which rarefaction value to extract
# #<plot.format> removes the time column to be in a proper plotting format
# #----
# extract.disp<-function(disp.data, rarefaction, plot.format=TRUE) {
#     #SANITIZING
#     #disparity
#     check.class(disp.data, "data.frame")
#     if(any(is.na(match(c("time", "rarefaction"), colnames(disp.data))))) {
#         stop("disp.data must have at least one column called 'time' and one called 'rarefaction'.")
#     }

#     #rarefaction
#     if(class(rarefaction) != 'numeric') {
#         #check.class(rarefaction, 'character')
#         if(rarefaction == "min") {
#             rar.val<-min(table(disp.data$time))+1
#             is.fun<-FALSE
#         } else {
#             if(rarefaction == "max") {
#                 is.fun<-TRUE
#             }
#         }
#     } else {
#         is.fun<-FALSE
#         rar.val<-rarefaction
#     }

#     #plot.format
#     #check.class(plot.format, 'logical')

#     #EXTRACTING THE RIGHT RAREFACTION VALUE

#     #Set the first row
#     sub_samp<-disp.data[which(disp.data$time == levels(disp.data$time)[1]),]
#     #Extract the rarefaction level
#     if(is.fun == TRUE) {
#         disp.data.sort<-sub_samp[which(sub_samp$rarefaction == max(sub_samp$rarefaction)),]
#     } else {
#         #Check if rarefaction level exists
#         if(length(which(sub_samp$rarefaction == rar.val)) == 1) {
#             #Extract the value
#             disp.data.sort<-sub_samp[which(sub_samp$rarefaction == rar.val),]
#         } else {
#             #If the rarefaction level doesn't exists, extract the max or min
#             if(all(rar.val > sub_samp$rarefaction)) {
#                 disp.data.sort<-sub_samp[which(sub_samp$rarefaction == max(sub_samp$rarefaction)),]
#             } else {
#                 disp.data.sort<-sub_samp[which(sub_samp$rarefaction == min(sub_samp$rarefaction)),]
#             }
#         }
#     }

#     #Do the same for the other levels
#     for (time in 2:length(levels(disp.data$time))) {
#         sub_samp<-disp.data[which(disp.data$time == levels(disp.data$time)[time]),]
#         if(is.fun == TRUE) {
#             new_line<-sub_samp[which(sub_samp$rarefaction == max(sub_samp$rarefaction)),]
#         } else {
#             #Check if rarefaction level exists
#             if(length(which(sub_samp$rarefaction == rar.val)) == 1) {
#                 #Extract the value
#                 new_line<-sub_samp[which(sub_samp$rarefaction == rar.val),]
#             } else {
#                 #If the rarefaction level doesn't exists, extract the max or min
#                 if(all(rar.val > sub_samp$rarefaction)) {
#                     new_line<-sub_samp[which(sub_samp$rarefaction == max(sub_samp$rarefaction)),]
#                 } else {
#                     new_line<-sub_samp[which(sub_samp$rarefaction == min(sub_samp$rarefaction)),]
#                 }
#             }
#         }
#     #bind the results
#     disp.data.sort<-rbind(disp.data.sort, new_line)
#     }

#     #Plot format?
#     #disp.data.sort$time<-NULL

#     return(disp.data.sort)
# }

# ##########################
# #read.data
# ##########################
# #reads the data given the chain_name, path, matrix, tree and disparity data.
# #----
# #WARNING: notice that this output function is out of the function environment (tree, table and dis_tmp).
# #----
# read.data<-function(chain_name, data_path, file_matrix, file_tree, disparity_data) {
#     #matrix
#     Nexus_data<-ReadMorphNexus(file_matrix)
#     Nexus_matrix<-Nexus_data$matrix
#     #tree
#     Tree_data<-read.nexus(file_tree)

#     ######################################
#     # Loading the data
#     ######################################

#     #Remove species with only missing data before hand
#     if(any(apply(is.na(Nexus_matrix), 1, all))) {
#         Nexus_matrix<-Nexus_matrix[-c(which(apply(is.na(Nexus_matrix), 1, all))),]
#     }

#     #Cleaning the tree and the table
#     #making the saving folder
#     tree<-clean.tree(Tree_data, Nexus_matrix)
#     table<-clean.table(Nexus_matrix, Tree_data)
#     Nexus_data$matrix<-table

#     #Forcing the tree to be binary
#     tree<-bin.tree(tree)

#     #Adding node labels and the age to the tree
#     tree<-lapply.root(tree, max(tree.age(tree)$age))

#     #Load the disparity data
#     name<-load(paste(data_path, chain_name, "/", chain_name, disparity_data, sep=""))
#     dis_tmp<-get(name)

#     return(list(table, tree, dis_tmp))
# }

# ##########################
# #mode.val
# ##########################
# #calculate the mode value of a distribution
# #----
# mode.val<-function (x) {
#     as.numeric(names(sort(-table(x))[1]))
# }