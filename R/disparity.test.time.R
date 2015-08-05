##########################
#disparity.test.time
##########################
#function for testing the effect of time on data
#v0.2
##########################
#SYNTAX :
#<data> can be either a list of matrices (e.g. for multivariate pco through time using permanova) or a time.disparity output (for univariate analysis, e.g. aov).
    #<force.test> optional, whether to force the test to be parametric or non parametric (can be "parametric", "non-parametric", or "NULL" (default))
#<method> distance method for the permanova (see adonis). If NULL method="euclidean".
#<permutations> number of permutations for the permanova (see adonis). If NULL permutations=1000.
#<...> any argument to pass to adonis
##########################
#Update: now only performs adonis
#----
#guillert(at)tcd.ie 11/07/2015
##########################

disparity.test.time<-function(data, method="euclidean", permutations=1000, ...) {

    #SANITIZING
    #data
    check.class(data, "list")
    if(length(data) < 2) {
        stop("Input data must have at least to time elements.")
    }

    #method
    check.class(method, "character")
    check.length(method, 1, " must be a distance method from vegdist().")
    methods_list<-c("manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis")
    if(all(is.na(match(method, methods_list)))) {
        stop("method must be a distance method from vegdist().")
    }

    #permutations
    check.class(permutations, "numeric")


    #TEST DIFFERENCES IN DISPARITY

    #Making the proper data format
    multi_data<-make.multivariate.table(data)
    data.values<-multi_data[[1]]
    time<-multi_data[[2]]

    #Running PERMANOVA
    multi_test<-adonis(data.values~time, method=method, permutations=permutations)
    message(paste("Tested input values ~ input time using PERMANOVA with ", method," distance and ", permutations, " permutations.", sep=""))


    #output
    return(multi_test)


    old_anova=FALSE
    #TIME: testing the effect of time on data
    if(old_anova == TRUE) {

        if(data_type == "Univariate") {
            #Univariate analysis (Kruskall Wallis or ANOVA)
            univ_data<-make.univariate.table(data)
            data.values<-univ_data[,1]
            time<-univ_data[,2]
        
            #Testing assumptions of anova
            test<-test.anova(univ_data)

            #Running the univariate test
            if(test[[1]] == TRUE) {
                if(force.test == "non-parametric") {
                    #KW test
                    univ_test<-kruskal.test(data.values~time)
                    message("Tested input values ~ input time using Kruskal-Wallis rank sum test.")
                } else {
                    #ANOVA
                    univ_test<-summary(aov(data.values~time))
                    message("Tested input values ~ input time using ANOVA.")
                    message("Residuals are normally distributed and variance within groups is homogeneous.")
                    message("Is the data independent? If not use the 'force.test=\"non-parametric\"' options.")
                }
            } else {
                if(force.test == "parametric") {
                    #ANOVA
                    univ_test<-summary(aov(data.values~time))
                    message("Tested input values ~ input time using ANOVA (parametric test was enforced using 'force.test=\"parametric\"' options).")
                    message("Residuals are NOT normally distributed and variance within groups is NOT homogeneous.")                
                } else {
                    #KW test
                    univ_test<-kruskal.test(data.values~time)
                    message("Tested input values ~ input time using Kruskal-Wallis rank sum test.")
                }

            }

            #Do test output
        }
    }
}


