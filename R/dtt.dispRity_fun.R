
## Modified .dtt function from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R
geiger.dtt.dispRity <- function(phy, data, metric, relative){

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
geiger.area.between.curves <- function(x, f1, f2, xrange=c(0,1)) {
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

## This is the internal function from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R (.ic.sigma)
geiger.ratematrix <- function(phy, data) {
    ## Sort the data cladewise (normally done by geiger::treedata but data is already cleaned by dispRity::clean.data)
    order <- match(rownames(data), phy$tip.label)
    rownames(data) <- phy$tip.label[order]
    index <- match(phy$tip.label, rownames(data))
    data <- as.matrix(data[index, ])
    if(length(dim(data)) == 2){
        data <- as.matrix(data)
    }
    td <- list(phy=phy, data=data)

    f <- function(x) pic(x, td$phy)
    ic <- apply(td$data, 2, function(x) {
                names(x)=rownames(td$data)
                f(x)
                })
    return(crossprod(ic, ic)/nrow(ic))
}

## This is the internal function from https://github.com/mwpennell/geiger-v2/blob/master/R/simulation.R
geiger.sim.char <- function(phy, par, nsim = 1, model = c("BM", "speciational", "discrete"), root=1) {
    model = match.arg(model, c("BM", "speciational", "discrete"))
    
    model.matrix = geiger.make.modelmatrix(par, model)    
    nbranches<-nrow(phy$edge)
    nspecies<-Ntip(phy)
    
    if(length(root)>1) stop("'root' should be a single value")
    
    if(model%in%c("BM", "speciational")) {

        m<-geiger.get.simulation.matrix(phy)
        if(model=="speciational") {
            m[m>0]<-1.0;
        }
        nchar<-nrow(model.matrix)
        rnd<-t(MASS::mvrnorm(nsim*nbranches, mu=rep(0, nchar), Sigma=model.matrix))
        rnd<-array(rnd, dim=c(nchar, nbranches, nsim));

        simulate<-function(v, root) {(m %*% as.matrix(v))+root;}
        
        result<-apply(rnd, 1, simulate, root)
        result<-aperm(array(result, dim=c(nspecies, nsim, nchar)), c(1, 3, 2))
        rownames(result)<-phy$tip.label
    } else {
        rt=nspecies+1
        zphy=reorder.phylo(phy, "postorder")
        el=zphy$edge.length
        nchar<-length(model.matrix);
        result<-array(0, dim=c(nspecies, nchar, nsim))
        .get.state=function(s, p){
            pp=cumsum(p[s,])
            min(which(runif(1)<pp))
        }
        for(j in 1:nchar) {
            m=model.matrix[[j]]
            if(!root%in%c(1:nrow(m))) stop(paste("'root' must be a character state from 1 to ", nrow(m), sep=""))
            p=lapply(el, function(l) matexpo(m*l))
            
            for(k in 1:nsim) {
                node.value<-numeric(nspecies+Nnode(zphy))
                node.value[rt]<-root
                for(i in nbranches:1) {
                    cur=zphy$edge[i,2]
                    anc=zphy$edge[i,1]
                    curp=p[[i]]
                    s=node.value[anc]
                    node.value[cur]=.get.state(s, curp)
                }
                result[,j,k]<-node.value[1:nspecies]
            }
        }
        rownames(result)<-zphy$tip.label;
        
    }
    return(result);
}

## This is the internal function from https://github.com/mwpennell/geiger-v2/blob/master/R/simulation.R
geiger.make.modelmatrix <- function(m, model=c("BM", "speciational", "discrete")){
    model=match.arg(model, c("BM", "speciational", "discrete"))
    if(model=="discrete"){
        if(is.matrix(m)){
            m=list(m)
            for(j in 1:length(m)){
                #.check.Qmatrix
                m=unique(dim(m[[j]]))
                if(length(m)>1) stop("'Q' must be a square matrix")
                didx=1 + 0L:(m - 1L) * (m + 1)
                if(!all(abs(rowSums(m[[j]]))<0.000001)) stop("rows of 'Q' must sum to zero")
                if(!all(m[[j]][didx]<=0)) stop("diagonal elements of 'Q' should be negative")
                if(!all(m[[j]][-didx]>=0)) stop("off-diagonal elements of 'Q' should be positive")
            }
        }
    } else {
        if(is.numeric(m)) m=as.matrix(m) else stop("Supply 'm' as a matrix of rates")
        if(any(diag(m)<0)) stop("'m' appears to have negative variance component(s)")
    }
    return(m)
}

## This is the internal function from https://github.com/mwpennell/geiger-v2/blob/master/R/simulation.R
geiger.get.simulation.matrix <- function(phy){
    N=Ntip(phy)
    n=nrow(phy$edge)
    ## Get mrca list by tip and node ID (equivalent of .cache.descendants()$tips from geiger)
    dd=c(as.list(1:Ntip(phy)), as.list(prop.part(phy)))
    m=matrix(0, N, n)
    edg=phy$edge.length
    idx=phy$edge[,2]
    for(x in 1:length(edg)){
        edge=edg[x]
        m[dd[[idx[x]]],x]=sqrt(edge)
    }
    return(m)
}
