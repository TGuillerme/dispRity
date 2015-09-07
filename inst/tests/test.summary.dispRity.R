
#Testing

#Case 1, no bootstrap
data<-test_data$ord_data_tips
data<-dispRity(data, metric=c(sum, range))

#Case 2, bootstraps
data<-test_data$ord_data_tips
data<-boot.matrix(data, bootstrap=5, rarefaction=FALSE, boot.type="full")
data<-dispRity(data, metric=c(sum, range))

#Case 3, bootstraps + rarefaction

#Case 4, time series

#Case 5, time series + bootstraps

#Case 5, time series + bootstraps + rarefaction
data<-test_data$ord_data_tips
factor<-as.data.frame(matrix(data=c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow=nrow(data), ncol=1))
rownames(factor)<-rownames(data)
data<-cust.series(data, factor)
data<-boot.matrix(data, bootstrap=5, rarefaction=c(5,6), boot.type="full")
data<-dispRity(data, metric=c(sum, range))