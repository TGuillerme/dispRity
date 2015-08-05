##########################
#disparity.test.age
##########################
#function for testing differences in disparity before and after a certain age
#v0.1
##########################
#SYNTAX :
#<pco_data> some pco data
#<time> at which time to split the data set
#<test.type> which type of analysis to run (Multivariate or univariate)
#<force.test> optional, whether to force the test to be parametric or non parametric (can be "parametric", "non-parametric", or "NULL" (default))
#<method> can be either the method to be passed to adonis (multivariate test) or the metric to be passed to time.disparity.
#<permutations> can be either the number of permutations to be passed to adonis (multivariate test) or the bootstraps to be passed to time.disparity.
#<...> arguments to pass to adonis or time.disparity
##########################
#----
#guillert(at)tcd.ie 08/07/2015
##########################

#DEBUG
#message("DEBUG ACTIVATED")
#time_pco<-pco_in_bins$pco_intervals
#time_dis<-disp_obs.dist.cent

#data<-time_pco
#type<-"Age"
#time=60


disparity.test.age<-function(pco_data, time, test.type, method, permutations, ...) {

    #SANITIZING
    stop("Function not developed yet.")
    message("Warning: no sanitizing in this version yet!\nUse this function at your own risks...")
    #Add data sanitizing input
    #pco

    #type
    type<-"Age"

    #Time

    #TIME CAN ALSO BE A STRATA
    #time (only in type = Age or Lag)
    check.class(time, "numeric")
    check.length(time, 1, " must be a single numerical value.")
    #check if time is present in the time_pco
    #Setting the data type
    if(data_type == "Multivariate") {
        #Multivariate data
        intervals<-names(data)
    } else {
        #Univariate data
        intervals<-names(data[[2]])
    }


    if(length(grep(time, intervals)) == 0) {
        #Time is within intervals
        stop("Not developed yet: time within intervals.")
    } else {
        if(length(grep(time, intervals))==2) {
            #Time is between intervals
            int_befor<-grep(time, intervals)[1]
            int_after<-grep(time, intervals)[2]
        } else {
            #Time is at interval
            int_befor<-grep(time, intervals)[1]
            int_after<-int_befor+1
        }
    }
    #Lag
    if(type == "Lag") {
        int_after<-int_after:length(intervals)
    }

    #test.type
    check.class(test.type, 'character')
    if(test.type != "univariate") {
        if(test.type != "multivariate") {
            stop("test.type must be either 'univariate', 'multivariate'.")
        }
    }      

    #Sanitizing for method and permutation

    #TEST DIFFERENCES IN DISPARITY
    #Age: testing difference between two time intervals
    if(type == "Age") {
        
        if(test.type == "multivariate") {
        #PERMANOVA
            perdata<-make.multivariate.table(make.age.interval(int_befor, int_after, data))
            data.values<-perdata[[1]]
            time<-perdata[[2]]
            multi_test<-adonis(data.values~time, method=method, permutations=permutations, ...)
        } else {
        #T-TEST

            #Making the data univariate (e.g. calculating the centroid distance)
            split_data<-make.age.interval(int_befor, int_after, data)
            calc.dis<-time.disparity(split_data, method=method, bootstraps=1000, CI=c(50,95), save.all=TRUE, ...)

            #Performing the Anderson-Friedman t-test
            
        }
    }

}


