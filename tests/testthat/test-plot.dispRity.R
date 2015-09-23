#TESTING summary.dispRity

# 
#Loading the data
load("test_data.Rda")
data<-test_data$ord_data_tips

#######################
#Testing
#######################

#Case 1, no bootstrap
data<-test_data$ord_data_tips
data<-dispRity(data, metric=c(sum, range))


#Case 2, bootstraps
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5)
data<-dispRity(data, metric=c(sum, range))

#Case 3, bootstraps + rarefaction
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=c(5,6))
data<-dispRity(data, metric=c(sum, range))

#Case 4, time series
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-dispRity(data, metric=c(sum, range))

#Case 5, time series + bootstraps
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5)
data<-dispRity(data, metric=c(sum, range))

#Case 5, time series + bootstraps + rarefaction
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5, rarefaction=TRUE)
data<-dispRity(data, metric=c(sum, range))