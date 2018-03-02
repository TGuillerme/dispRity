
## Modified .dtt function from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R
.dtt.dispRity <- function(phy, data, metric){

    phy$node.label<-NULL
    td<-geiger::treedata(phy, data)
    phy2<-td$phy
    phy<-new2old.phylo(td$phy)

    result<-numeric()


    node.depth<-branching.times(phy2);
    stem.depth<-numeric();
    stem.depth[1]<-node.depth[1];
    for(i in 2:phy2$Nnode) {
        anc<-which(as.numeric(phy$edge[,2])==-i)
        stem.depth[i]<-node.depth[names(node.depth)==phy2$edge[anc,1]]
    }

    ltt<-sort(node.depth, decreasing=TRUE)
    node.depth<-node.depth/max(ltt);
    stem.depth<-stem.depth/max(ltt);
    ltt<-ltt/max(ltt);
    if(length(dim(td$data))==2) {


        #d<-disparity(phy2, td$data, index=disp)
        d <- as.vector(summary(dispRity(custom.subsets(td$data, phy2), metric = metric), digits = 10)$obs)
        names(d) <- Ntip(phy2)+1:Nnode(phy2)
        

        result[1]<-d[1]
        for(i in 2:length(ltt)) {
            x<-d[stem.depth>=ltt[i-1]&node.depth<ltt[i-1]]
            if(length(x)==0) result[i]=0
            else result[i]<-mean(x);
        }
        result[length(ltt)+1]<-0;
        if(result[1]>0)
        result<-result/result[1];

    } else {

        if(length(dim(td$data))!=3)
        stop("Error in data");

        for(i in 1:dim(td$data)[3]) {

            pp <- as.matrix(td$data[,,i])


            # d <- disparity(phy2, pp, index=disp)
            d <- summary(dispRity(custom.subsets(pp, phy2), metric = metric), digits = 10)$obs
            

            y<-numeric()

            y[1]<-d[1]
            for(j in 2:length(ltt)) {
                x<-d[stem.depth>=ltt[j-1]&node.depth<ltt[j-1]]
                if(length(x)==0) y[j]=0
                else y[j]<-mean(x);
            }
            y[length(ltt)+1]<-0;
            if(y[1]>0)
            y<-y/y[1];

            result<-cbind(result, y)
        }
    }

    return(result);
}

## This is the internal function from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R
.area.between.curves <-
function(x, f1, f2, xrange=c(0,1))
{
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

## This is the internal function from https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R
getMDIp<-function(dttRes) {
    foo<-function(x) {
        return(.area.between.curves(x= dttRes$times, f1=x, f2=dttRes$dtt))
    }
    mdis<-apply(dttRes$sim,2,foo)
    pVal<-length(which(mdis>=0))/length(mdis)
    return(pVal)
}