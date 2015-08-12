time.series<-function(data, tree, method, time, model, inc.nodes, FADLAD, verbose=FALSE) {
    
    message("time.series: UNTESTED")

    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #data must be a matrix
    check.class(data, 'matrix')
    #data must be of size k*<=k-1
    if(nrow(data) < (ncol(data) - 1)) stop("Input data must have at least k-1 columns")

    #TREE (1)
    #tree must be a phylo object
    check.class(tree, 'phylo')
    #tree must be dated
    if(length(tree$root.time) == 0) stop("Tree must be a dated tree with $root.time.")

    #METHOD
    #method must be a character string
    check.class(method, "character")
    #method must have only one element
    check.length(method, 1, 'must be either "discrete", "d", "continuous", or "c".')
    #method must be either "discrete", "d", "continuous", or "c"
    all_methods <- c("discrete", "d", "continuous", "c")
    if(all(is.na(match(method, all_methods)))) stop('method must be either "discrete", "d", "continuous", or "c".')
    
    #if method is "d" or "c", change it to "discrete" or "continuous" (lazy people...)
    if(method == "d") method <- "discrete"
    if(method == "c") method <- "continuous"

    #TIME
    #time must be numeric of integer
    if(class(time) != 'numeric') {
        if(class(time) != 'integer') stop("time must be numeric.")
    }
    #If time is a single value create the time vector by sampling evenly from just after the tree root time (1%) to the present
    if(length(time) == 1) {
        #time must be at least 3 if discrete
        if(method == "discrete" & time < 3) stop("If method is discrete, time must be at least 3.")
        #or at least time 2 if continuous
        if(method == "continuous" & time < 2) stop("If method is discrete, time must be at least 2.")
        #Create the time vector
        time <- seq(from=0, to=tree$root.time-0.01*tree$root.time, length.out=time)
    }
    #time cannot be older than the root age
    if(any(time >= tree$root.time)) stop("Time cannot be older or equal to the tree's root age.")
    #time vector must go from past to present
    if(time[1] < time[2]) time <- rev(time)

    #MODEL
    #if method is discrete ignore model
    if(method == "discrete") {
        if(missing(model)) {
            model <- NULL
        } else {
            warning("model is ignored if method is 'discrete'.")
            model <- NULL
        }
    } else {
    #else model must be one of the following
        all_models <- c("acctran", "deltran", "punctuated", "gradual")
        if(all(is.na(match(model, all_models)))) stop('model must be either "acctran", "deltran", "punctuated" or "gradual".')
            #~~~~~~~~~~~
            # Include the make.model option here?
            # make.model should be tested on slice.tree function
            #~~~~~~~~~~~
    }

    #INC.NODES
    #if method is continuous set inc.nodes to TRUE.
    if(method == "continuous") {
        if(missing(inc.nodes)) {
            inc.nodes <- TRUE
        } else {
            warning("inc.nodes is ignored if method is 'continuous")
            inc.nodes <- TRUE
        }
    } else {
    #else inc.nodes must be logical
        check.class(inc.nodes, 'logical')
    }

    #TREE (2)
    #If inc.nodes is TRUE, tree nodes and labels must match data rows
    if(inc.nodes == TRUE) {
        #Check if the tree has node labels
        if(length(tree$node.label) != 0) {
            #Check if the tree and the table are the same length
            if(nrow(data) != (Ntip(tree)+Nnode(tree))) stop('The labels in the table and in the tree do not match!\nCheck especially the node labels in the tree and the table.')
            #Check if both nodes and tip labels match with the data rownames
            if(any(is.na(rownames(data), c(tree$tip.label, tree$node.label)))) stop('The labels in the table and in the tree do not match!\nCheck especially the node labels in the tree and the table.')
        } else {
            stop('The labels in the table and in the tree do not match!\nCheck especially the node labels in the tree and the table.')
        }
    } else {
        #else check if the tree and the table are the same length
        if(nrow(data) != Ntip(tree)) stop('The labels in the table and in the tree do not match!')
        #Also check if the names are identical
        if(any(is.na(match(rownames(data), tree$tip.label)))) stop('The labels in the table and in the tree do not match!')
    }


    #FADLAD
    if(missing(FADLAD)) {
        #If missing, create the FADLAD table
        FADLAD <- data.frame("FAD"=tree.age(tree)[1:Ntip(tree),1], "LAD"=tree.age(tree)[1:Ntip(tree),1], row.names=tree.age(tree)[1:Ntip(tree),2])
        message("No FADLAD table has been provided so every tip is assumed to interval single points in time.")
    } else {
        #Check if the FADLAD contains all taxa
        if(any(tree$tip.label %in% rownames(FADLAD) == FALSE)) {
            message("Some tips have FAD/LAD and are assumed to interval single points in time.")
            #If not generate the FADLAD for the missing taxa
            missing_FADLAD<-which(is.na(match(tree$tip.label, rownames(FADLAD))))
            add_FADLAD<-data.frame(tree.age(tree)[missing_FADLAD,1], tree.age(tree)[missing_FADLAD,1], row.names=tree.age(tree)[missing_FADLAD,2])
            colnames(add_FADLAD)<-colnames(FADLAD)
            FADLAD<-rbind(FADLAD, add_FADLAD)
        }
        #Remove FADLAD taxa not present in the tree
        if(nrow(FADLAD) != Ntip(tree)) {
            FADLAD<-FADLAD[-c(which(is.na(match(rownames(FADLAD), tree$tip.label)))),]
        }
    }

    #VERBOSE
    check.class(verbose, 'logical')

    #----------------------
    # GENRATING THE TIME SERIES
    #----------------------

    if(method == "discrete") {
        time_series<-time.series.discrete(data, tree, time, FADLAD, inc.nodes)
    }

    if(method == "discrete") {
        time_series<-time.series.continuous(data, tree, time, model, FADLAD, verbose)
    }

    return(time_series)

}
