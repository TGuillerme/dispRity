
## Modified .dtt function from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R
.dtt.dispRity <- function(phy, data, metric, relative){

    ## Combining the tree and the data
    phy$node.label <- NULL
    td <- list("phy" = phy, "data" = data) #TG: data is already cleaned prior to the function
    phy2 <- td$phy
    ## Converting to old phylo format
    phy <- new2old.phylo(td$phy)

    ## Getting the node depth from ape if no attributes
    if(is.null(phy2$root.time)){
        node.depth <- branching.times(phy2)
    } else {
        node.depth <- tree.age(phy2)$ages[-c(1:Ntip(phy2))]
        names(node.depth) <- 1:length(node.depth) + Ntip(phy2)
    }

    stem.depth <- numeric()
    stem.depth[1] <- node.depth[1]
    for(i in 2:phy2$Nnode) {
        anc <- which(as.numeric(phy$edge[,2]) == -i)
        stem.depth[i] <- node.depth[names(node.depth) == phy2$edge[anc,1]]
    }

    ## Lineages through time
    ltt <- sort(node.depth, decreasing = TRUE)

    ## Scaling by lineage through time
    node.depth <- node.depth/max(ltt)
    stem.depth <- stem.depth/max(ltt)
    ltt <- ltt/max(ltt)

    result <- numeric()

    ## By matrix
    if(length(dim(td$data)) == 2) {

        ## Calculate disparity per clade
        disparity <- as.vector(summary(dispRity(custom.subsets(td$data, phy2), metric = metric), digits = 10)$obs)
        names(disparity) <- 1:length(node.depth) + Ntip(phy2)
        
        ## Disparity at the root
        result[1] <- disparity[1]

        ## Disparity for the other nodes (average per time slice)
        for(i in 2:length(ltt)) {
            x <- disparity[stem.depth >= ltt[i-1] & node.depth < ltt[i-1]]
            #result[i] <- ifelse(length(x) == 0, 0, mean(x))                             #TODO: check 0 for non-fossil tree
            if(length(x) == 0) {
                result[i] <- 0
            } else {
                result[i] <- mean(x)
            }
        }

        result[length(ltt)+1] <- 0                                                      #TODO: check 0 for non-fossil tree

        ## Scaling the results
        if(result[1] > 0){                                                              #TODO: check 0 for non-fossil tree
            result <- result/result[1]
        }

    } else {
        ## By array

        if(length(dim(td$data)) != 3){
            stop("Error in data: must be a matrix or a array of matrix (length(dim(data)) must be equal to 2 or 3).")
        }

        ## Looping through the array
        for(i in 1:dim(td$data)[3]) {

            one_matrix <- as.matrix(td$data[,,i])

            ## Calculate disparity per clade
            disparity <- summary(dispRity(custom.subsets(one_matrix, phy2), metric = metric), digits = 10)$obs
            

            y <- numeric()

            y[1] <- disparity[1]
            for(j in 2:length(ltt)) {
                x <- disparity[stem.depth >= ltt[j-1] & node.depth < ltt[j-1]]
                # y[j] <- ifelse(length(x) == 0, 0, mean(x))                                  #TODO: check 0 for non-fossil tree
                if(length(x) == 0) {
                    y[j] <- 0
                } else {
                    y[j] <- mean(x)
                }
            }

            y[length(ltt) + 1] <- 0                                                         #TODO: check 0 for non-fossil tree
            if(y[1] > 0){
                y <- y/y[1]
            }
            
            result <- cbind(result, y)
        }
    }

    return(result);
}

## This is the internal function from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R
.area.between.curves <- function(x, f1, f2, xrange=c(0,1)) {
    a<-0.0;
    for(i in 1:length(x)) {
        if(x[i]>=xrange[1] & x[i]<=xrange[2]) {
            if(i==1) {
                lhs<-0
            } else if(x[i-1]<xrange[1]) {
                lhs<-xrange[1]
            } else lhs<-x[i-1];
            if(i==length(x)) {
                rhs<-x[i]
            } else if(x[i+1]>xrange[2]) {
                rhs<-xrange[2];
            } else rhs<-x[i+1];
            a<-a+(f2[i]-f1[i])*(rhs-lhs)/2;
        } else if(i!=1) if(x[i-1]>=xrange[1] & x[i-1]<=xrange[2]) {
            y1<-f1[i-1]+(f1[i]-f1[i-1])*(xrange[2]-x[i-1])/(x[i]-x[i-1])
            y2<-f2[i-1]+(f2[i]-f2[i-1])*(xrange[2]-x[i-1])/(x[i]-x[i-1])
            a<-a+(y2-y1)*(xrange[2]-x[i-1])/2;
        } else if(i!=length(x)) if(x[i+1]>=xrange[1] & x[i+1]<=xrange[2]) {
            y1<-f1[i]+(f1[i+1]-f1[i])*(xrange[1]-x[i])/(x[i+1]-x[i])
            y2<-f2[i]+(f2[i+1]-f2[i])*(xrange[1]-x[i])/(x[i+1]-x[i])

            a<-a+(y2-y1)*(x[i+1]-xrange[1])/2;
        }
    }
    return(a)
}