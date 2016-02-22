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
Bootstrap.rarefaction<-function(data, bootstraps, rarefaction, boot.type) {
    #This function is based on DisparityCalc() from Smith et al. 2014 - Evolution (http://dx.doi.org/10.1111/evo.12435) http://datadryad.org/resource/doi:10.5061/dryad.d380g 
    #Set rarefaction (or not)
    if(rarefaction[1] == TRUE) {
        rarefaction_max<-seq(from=3, to=nrow(data))
    } else {
        if((class(rarefaction) == "numeric") | (class(rarefaction) == "integer")) {
            rarefaction_max<-rarefaction
        } else {
            if(class(rarefaction) == "integer") {

            }
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
        no.BS<-FALSE
    }

    #Set the bootstrap method
    #boot.type="single"

    for(rare in rarefaction_max){
        #Bootstraps
        for(BS in 1:bootstraps){ #bootstraps -> bootstraps
            #Bootstrap 
            if(no.BS==TRUE) {
                output<-data[sample(1:nrow(data),rare,FALSE),]
            } else {

                if(class(boot.type) == "function") {
                    output<-boot.type(data, rare)
                } else {
                    if(boot.type=="full") {
                        output<-boot.full(data, rare)
                    }
                    
                    if(boot.type=="single") {
                        output<-boot.single(data, rare)
                    }

                    if(boot.type=="frac") {
                        #Or use method in between? 10% of resampling?
                        stop("Fractional bootstrap under development.\nSee internal function disparity/disparity_fun/Bootstrap.rarefaction.")
                    }
                }

            }
            result[BS] <- list(output)
        }
        #Rarefaction + BS results
        BSresult[rare]<-list(result)
    }

    #Remove two first element if rarefaction
    if(rarefaction[1] == TRUE) {
        BSresult[[2]]=NULL
        BSresult[[1]]=NULL
    } else {
        #Removing the null elements
        nulls<-unlist(lapply(BSresult, is.null))==FALSE
        BSresult<-BSresult[nulls]
    }
    return(BSresult)
}