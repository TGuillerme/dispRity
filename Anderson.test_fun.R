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
