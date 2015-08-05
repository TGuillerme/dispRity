#FUNCTIONS FOR DISPARITY.TEST

#Making the univariate table (e.g. for aov)
make.univariate.table<-function(time_disparity) {
    #Extracting the list of values
    list_val<-time_disparity$values

    #Extracting the values
    val_uni<-unlist(list_val)
    #Removing the names
    names(val_uni)<-NULL

    #Extracting the time
    time_counts<-unlist(lapply(list_val, lapply, length))
    #Creating the time vector
    time<-as.factor(rep(names(list_val), time_counts))

    #Returning the results
    output<-cbind(val_uni, as.factor(time))
    output<-as.data.frame(output)
    output[,2]<-as.factor(time)
    names(output)<-c('Values', 'Time')
    return(output)
}
#next: summary(aov(bla$Values~bla$Time))


#Making the multivariate table (e.g. for adonis)
make.multivariate.table<-function(time_pco) {
    #Binding the first two elements
    pco_mat<-rbind(time_pco[[1]], time_pco[[2]])

    #Binding the other elements
    if(length(time_pco) > 2) {
        for (interval in 3:length(time_pco)) {
            pco_mat<-rbind(pco_mat, time_pco[[interval]])
        }
    }
    
    #Extracting the time
    time_counts<-unlist(lapply(time_pco, nrow))
    #Creating the time vector
    time<-as.factor(rep(names(time_pco), time_counts))

    #Returning the table
    output<-list(pco_mat, time)
    names(output)<-c("Data", "Time")
    return(output)
}
#next: adonis(bla~bla, method='euclidean', permutations=1000)

#Testing assumptions of anova
test.anova<-function(data) {
    #anova
    anova<-aov(data[,1]~data[,2])

    #residuals
    normal_res<-shapiro.test(rstandard(anova))

    #variance
    var_test<-bartlett.test(data[,1]~data[,2])

    #Test
    if(normal_res$p.value > 0.05 & var_test$p.value > 0.05) {
        test_results<-TRUE
    } else {
        test_results<-FALSE
    }

    #Saving the test table
    test.save<-data.frame(row.names=c("Shapiro","Bartlett"), statistic=c(normal_res$statistic[[1]], var_test$statistic[[1]]), df=c(NA, var_test$parameter[[1]]), p.value=c(normal_res$p.value[[1]], var_test$p.value[[1]]))

    #output
    output<-list("Pass"=test_results, "Test"=test.save)
    return(output)
}

#Making the two time intervals groups for multivariate data
make.age.interval<-function(int_befor, int_after, data) {
    #Setting the intervals
    interval_1<-seq(1:int_befor)
    interval_2<-seq(from=int_after, to=length(data))

    #Regroup the data per interval
    interval_bef<-data[[interval_1[1]]]
    if(length(interval_1) > 1) {
        for (n in 2:length(interval_1)) {
            interval_bef<-rbind(interval_bef, data[[interval_1[n]]])
        }
    }
    interval_bef<-interval_bef[unique(rownames(interval_bef)), ]

    interval_aft<-data[[interval_2[1]]]
    if(length(interval_2) > 1) {
        for (n in 2:length(interval_2)) {
            interval_aft<-rbind(interval_aft, data[[interval_2[n]]])
        }
    }
    interval_aft<-interval_aft[unique(rownames(interval_aft)), ]

    #Regrouping the intervals in the same list
    return(list("before"=interval_bef, "after"=interval_aft))
}

Anderson.test<-function(BSresults, time_pco) {
    #Disparity T-Test calculation from Anderson and Friedman 2012.
    #Code modified from Smith et al. 2014 - Evolution

    #Extracting the sample sizes
    sample_size<-unlist(lapply(time_pco, nrow))

    #Getting the mean and the variance from the BSresults
    variance_int<-apply(BSresults, 2, var)
    mean_int<-apply(BSresults, 2, mean)

    #Calculating the T_statistics functions
    mean.difference<-function(x,y, mean_int) {mean_int[x]-mean_int[y]}
    term.A<-function(x,y,sample_size, variance_int) { ((sample_size[x]-1)*(sample_size[x])* variance_int[x] + (sample_size[y]-1)*(sample_size[y])* variance_int[y] )/(sample_size[x]+sample_size[y]+2) }
    term.B<-function(x,y, sample_size) { (sample_size[x] + sample_size [y])/(sample_size[x] * sample_size [y]) }

    #Calculating the statistic, df and p-value.
    difference<-p_values<-degrees_freedom<-t_statistic<-as.data.frame(matrix(NA, nrow=ncol(BSresults), ncol=ncol(BSresults)))
    rownames(difference)<-rownames(p_values)<-rownames(degrees_freedom)<-rownames(t_statistic)<-names(time_pco)
    colnames(difference)<-colnames(p_values)<-colnames(degrees_freedom)<-colnames(t_statistic)<-names(time_pco)
    for(row in 1:ncol(BSresults)) {
        for(col in 1:ncol(BSresults)) {
            #Calculate difference
            difference[row,col]<-mean.difference(row,col, mean_int)
            #Calculate T
            t_statistic[row,col]<-mean.difference(row,col, mean_int)/sqrt(term.A(row,col,sample_size,variance_int)*term.B(row,col,sample_size))
            if(!is.finite(t_statistic[row,col])) {
                #Exist the loop if some variance or differences are not finit numbers.
                message("T statistic cannot be calculate. Probable reason: some values are is near Inf or -Inf.")
                return(list("diff"=difference, "df"=degrees_freedom, "T"=t_statistic, "p"=p_values))
            }

            #Calculate df
            degrees_freedom[row,col]<-sample_size[row]+sample_size[col]-2
            #Calculate p
            p_values[row,col]<- 1-pt(t_statistic[row, col], df = degrees_freedom[row, col])

            #make test two-tailed
            if (p_values [row,col] > 0.5) {
                p_values [row,col] <- 2*(1-p_values[row,col])
            } else {
                if (p_values [row,col] < 0.5){
                    p_values [row,col] <- 2*(p_values[row,col])   
                } else {
                    if (p_values [row,col] == 0.5){
                        p_values [row,col] <- 1
                    }
                }
            }
        }
    }
    return(list("diff"=difference, "df"=degrees_freedom, "T"=t_statistic, "p"=p_values))
}

#P-value significance levels
apply.signif<-function(p.value) {
    if(p.value < 0.001) {
        p<-"***"
    } else {
        if(p.value < 0.01) {
            p<-"**"
        } else {
            if(p.value < 0.05) {
                p<-"*"
            } else {
                if(p.value < 0.1) {
                    p<-"."
                } else {
                    if(p.value > 0.1) {
                        p<-" "
                    }
                }
            }
        }
    }
    return(p)
}

#Getting the significance tokens
signif.token<-function(p.values) {
    output<-vector()
    for(val in 1:length(p.values)) {
        output<-c(output, apply.signif(p.values[val]))
    }
    return(output)
}

#Capitalize words
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}