#FUNCTIONS FOR DISPARITY

#Bootstrap methods algorithm
#Full bootstrap: full resampling method: for each bootstrap, resample all the rows from the available rows (for n rows, can randomly resample n times the same row).
boot.full<-function(data, rare) {
    output<-as.matrix(data[sample(1:nrow(data),rare,TRUE),])
    return(output)
}

 #Single bootstrap: for each bootstrap, select one row and replace it by a randomly chosen left one (for n rows, only one row can be present two times).
boot.single<-function(data, rare) {
    #First remove n row if rarefaction is true
    output<-data[sample(1:nrow(data),rare,FALSE),]
    #Then randomly chose a row to remove and to replace (different)
    row.out.in<-sample(1:nrow(output),2,FALSE)
    #Finally replace the selected row out by the selected row in 
    output[row.out.in[1],]<-output[row.out.in[2],] ; rownames(output)[row.out.in[1]]<-rownames(output)[row.out.in[2]]
    return(output)
}

#Performs bootstrap and eventual rarefaction
Bootstrap.rarefaction<-function(data, bootstraps, rarefaction, boot.method="single") {
    #This function is based on DisparityCalc() from Smith et al. 2014 - Evolution (http://dx.doi.org/10.1111/evo.12435) http://datadryad.org/resource/doi:10.5061/dryad.d380g 
    #Set rarefaction (or not)
    if(rarefaction == TRUE) {
        rarefaction_max<-seq(1:nrow(data))
        rarefaction_max<-rarefaction_max[-c(1,2)]
    } else {
        if(class(rarefaction) == "numeric") {
            rarefaction_max<-rarefaction
        } else {
            rarefaction_max<-nrow(data)
        }
    }
    #Rarefaction
    result<-NULL
    BSresult<-NULL
    #Set bootstraps to 1 if bootstraps=0 with no replacement
    if(bootstraps==0) {
        bootstraps<-1
        no.BS<-TRUE
    } else {
        no.BS<-FALSE}

    #Set the bootstrap method
    #boot.method="single"

    for(rare in rarefaction_max){
        #Bootstraps
        for(BS in 1:bootstraps){ #bootstraps -> bootstraps
            #Bootstrap 
            if(no.BS==TRUE) {
                output<-data[sample(1:nrow(data),rare,FALSE),]
            } else {

                if(boot.method=="full") {
                    output<-boot.full(data, rare)
                }
                
                if(boot.method=="single") {
                    output<-boot.single(data, rare)
                }

                if(boot.method=="frac") {
                    #Or use method in between? 10% of resampling?
                    stop("Fractional bootstrap under development.\nSee internal function disparity/disparity_fun/Bootstrap.rarefaction.")
                }

            }
            result[BS] <- list(output)
        }
        #Rarefaction + BS results
        BSresult[rare]<-list(result)
    }

    #Remove two first element if rarefaction
    if(rarefaction == TRUE) {
        BSresult[[2]]=NULL
        BSresult[[1]]=NULL
    } else {
        #Removing the n-1 first elements
        BSresult<-BSresult[-c(1:(rarefaction_max-1))]
    }
    return(BSresult)
}

#Range Calculations
range.calc<-function(list_table) {
    #This function is based on DisparityCalc() from Smith et al. 2014 - Evolution (http://dx.doi.org/10.1111/evo.12435) http://datadryad.org/resource/doi:10.5061/dryad.d380g 
    #Empty matrix (for output)
    output<-matrix(nrow=length(list_table), ncol=ncol(list_table[[1]]))
    #Looping through columns and rows
    for(row in 1:length(list_table)) { #Rows are bootstraps
        for(column in 1:ncol(list_table[[1]])) { #Columns are axes
            output[row,column]<-max(list_table[[row]][,column])-min(list_table[[row]][,column])
        }
    }
    return(output)
}

#Variance calculation
variance.calc<-function(list_table) {
    #This function is based on DisparityCalc() from Smith et al. 2014 - Evolution (http://dx.doi.org/10.1111/evo.12435) http://datadryad.org/resource/doi:10.5061/dryad.d380g 
    #Empty matrix (for output)
    output<-matrix(nrow=length(list_table), ncol=ncol(list_table[[1]]))
    #Looping through columns and rows
    for(row in 1:length(list_table)) { #Rows are bootstraps
        for(column in 1:ncol(list_table[[1]])) { #Columns are axes
            output[row,column]<-var(list_table[[row]][,column])
        }
    }
    return(output)
}

#Set-up for the NthRoot function in order to scale your products correctly.
nth.root<-function(x, n){
    x^(1/n)
}

#Apply loop for calculating the product
prod.apply<-function(X) {
    output<-nth.root(apply(X,1,prod), n=ncol(X))
    return(output)
}

#Apply loop for calculating the sum
sum.apply<-function(X) {
    output<-apply(X,1,sum)
    return(output)
}

#No apply (does nothin)
no.apply<-function(X) {
    return(X)
}

#Apply loops for extracting the central tendency of the centroids
cen.apply.med<-function(X) {
    return(apply(X, 1, median))
}
cen.apply.mea<-function(X) {
    return(apply(X, 1, mean))
}

#Apply loop for calculating the centroid
centroid.apply<-function(X) {
    #Euclidean distances to the centroid
    #This function is based on euc.dist.cent() from Finlay & Cooper 2015 - PeerJ (https://github.com/SiveFinlay/Diversity_Paper/blob/master/functions/Morpho_diversity_functions.r) 
    #Centroid (mean score of each PC axis)
    centroid<-apply(X, 2, mean)
        #Outputs length(X) values that are the mean value of each eigenvector (X)

    #Euclidean distances to the centroid
    cent.dist<-NULL
    for (j in 1:nrow(X)){
        cent.dist[j] <- dist(rbind(X[j,], centroid), method="euclidean")
            #Outputs 1 value that is the euclidean distance between the scores of taxa j for each eigenvector and the mean of each eigenvector? 
    }
    return(cent.dist)
}


#Lapply loop for calculating the centroid
centroid.calc<-function(X) {
    Y<-lapply(X, centroid.apply)
    return(matrix(nrow=length(X), data=unlist(Y), byrow=TRUE))
}

dispersion.apply<-function(X) {

    warning("Dispersion calculation in development.")

    #Centroid (mean score of each PC axis)
    centroid<-apply(X, 2, mean)

    dispersion<-X
    for(n in 1:length(centroid)) {
        #Euclidean distance from each data point to the centroid in each dimension
        dispersion[,n]<-sqrt( ( X[,n]-centroid[n] )^2 )
    }

    #Calculating the mean spread (mean distance to the centroid across all the dimensions)
    mean.dispersion<-apply(dispersion, 1, mean)

    return(list(mean.dispersion, dispersion))
}

#Lapply loop for calculating the dispersion
dispersion.calc<-function(X, save.all) {
    
    warning("Dispersion calculation in development.")

    Y<-lapply(X, dispersion.apply)
    mean.Y<-Y[][[1]]
    #mean.dispersions<-matrix(nrow=length(X), data=unlist(Y), byrow=TRUE)
    return(matrix(nrow=length(X), data=unlist(Y), byrow=TRUE))
}


#Volume calculation
volume.fast<-function(X, eigen.val) {
    #Correct calculation of the volume (using the eigen values)
    #remove the eigen values for the eigen vectors not present in X
    eigen.val<-eigen.val[1:ncol(X)]

    #dimensionality (where k (or n in Donohue et al 2013) is the size of the covariance matrix but if corrected is the size of the covariance matrix - 2 (n=k-2))
    n<-ncol(X)
    #volume
    vol<-pi^(n/2)/gamma((n/2)+1)*prod(eigen.val^(0.5))
    return(vol)

    #For volume through time use the eigenvectors??
}

volume<-function(X) {
    #Calcualtion of the volume by using the variance/covariance matrix (slightly slower)
    #dimensions
    n<-ncol(X)
    #distance matrix size
    k<-nrow(X)

    #The eigen value is equal to the sum of the variance/covariance within each axis multiplied by the maximum number of dimensions (k-1)
    eigen.value<-abs(apply(var(X),2, sum)*(k-1))

    #volume
    vol<-pi^(n/2)/gamma((n/2)+1)*prod(eigen.value^(0.5))
    return(vol)
}

volume.calc<-function(X) {
    Y<-lapply(X, volume)
    return(unlist(Y))
}

#volumes<-unlist(lapply(BSresult, volume.calc))


#Converts one or more CI into a quantile probabilities
CI.converter<-function(CI) {
    sort(c(50-CI/2, 50+CI/2)/100)
}

#Calculate product for the central tendency and the CIs for variance or range
Disparity.measure.table<-function(type_function, distribution_variable, central_tendency, CI, save.all) {

    #Products/Sum of distribution_variable (correct by the NthRoot)
    Disparity_measure<-lapply(distribution_variable, type_function)
    #Confidence intervals for the products/sum of distribution_variable
    Disparity_measure_quantile<-lapply(Disparity_measure, quantile, probs=CI.converter(CI))
    #Calculate the central tendency
    Disparity_measure_central<-lapply(Disparity_measure, central_tendency)

    #Transform the results into a data.frame
    #Add just the first column (central tendency)
    Disparity_measure_table<-data.frame("Central"=unlist(Disparity_measure_central))
    #Add the CIs
    Disparity_measure_table<-cbind(Disparity_measure_table, matrix(ncol=length(CI)*2, data=unlist(Disparity_measure_quantile), byrow=TRUE))
    #Add the CIs names
    colnames(Disparity_measure_table)[-1]<-paste(CI.converter(CI)*100, "%", sep="")

    #return only the quantile table
    if(save.all == FALSE) {
        return(Disparity_measure_table)
    } else {
        output<-list("quantiles"=Disparity_measure_table, "values"=Disparity_measure)
        return(output)
    }

}
