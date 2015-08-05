#TEST DISPARITY

#functions

#Testing Bootstrap.Rarefaction
set.seed(1)
data<-matrix(nrow=10, ncol=10, data=rnorm(100))
testBS<-Bootstrap.rarefaction(data, bootstraps=10, rarefaction=FALSE)
testBSR<-Bootstrap.rarefaction(data, bootstraps=10, rarefaction=TRUE)

#test
expect_is(testBS, 'list') ; message('.', appendLF=FALSE)
expect_is(testBSR, 'list') ; message('.', appendLF=FALSE)
expect_equal(length(unlist(testBS)), 1000) ; message('.', appendLF=FALSE)
expect_equal(length(testBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(testBSR)), 5400) ; message('.', appendLF=FALSE)
expect_equal(length(testBSR), 9) ; message('.', appendLF=FALSE)

#Testing range.calc
test_rangeBS<-lapply(testBS, range.calc)
test_rangeBSR<-lapply(testBSR, range.calc)

#test
expect_is(test_rangeBS, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test_rangeBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_rangeBS)), 100) ; message('.', appendLF=FALSE)
expect_is(test_rangeBSR, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test_rangeBSR), 9) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_rangeBSR)), 900) ; message('.', appendLF=FALSE)

#Testing variance.calc
test_varianceBS<-lapply(testBS, variance.calc)
test_varianceBSR<-lapply(testBSR, variance.calc)

#test
expect_is(test_varianceBS, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test_varianceBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_varianceBS)), 100) ; message('.', appendLF=FALSE)
expect_is(test_varianceBSR, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test_varianceBSR), 9) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_varianceBSR)), 900) ; message('.', appendLF=FALSE)

#Testing NthRoot
test_NthRoot<-matrix(nrow=2,ncol=2,data=1)
test_NthRoot1<-NthRoot(test_NthRoot)
test_NthRoot2<-NthRoot(data)

#test
expect_is(test_NthRoot1, 'matrix') ; message('.', appendLF=FALSE)
expect_is(test_NthRoot2, 'matrix') ; message('.', appendLF=FALSE)
expect_equal(length(test_NthRoot1), 4) ; message('.', appendLF=FALSE)
expect_equal(length(test_NthRoot2), 100) ; message('.', appendLF=FALSE)
expect_equal(sum(test_NthRoot1), 4) ; message('.', appendLF=FALSE)
expect_true(any(is.na(test_NthRoot2))) ; message('.', appendLF=FALSE)

#Testing prod.apply
test_prod.applyBS<-lapply(test_rangeBS, prod.apply)
test_prod.applyBSR<-lapply(test_rangeBSR, prod.apply)

#test
expect_is(test_prod.applyBS, 'list') ; message('.', appendLF=FALSE)
expect_is(test_prod.applyBSR, 'list') ; message('.', appendLF=FALSE)
expect_equal(length(test_prod.applyBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(test_prod.applyBSR), 9) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_prod.applyBS)), 10) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_prod.applyBSR)), 90) ; message('.', appendLF=FALSE)

#Testing sum.apply
test_sum.applyBS<-lapply(test_rangeBS, sum.apply)
test_sum.applyBSR<-lapply(test_rangeBSR, sum.apply)

#test
expect_is(test_sum.applyBS, 'list') ; message('.', appendLF=FALSE)
expect_is(test_sum.applyBSR, 'list') ; message('.', appendLF=FALSE)
expect_equal(length(test_sum.applyBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(test_sum.applyBSR), 9) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_sum.applyBS)), 10) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_sum.applyBSR)), 90) ; message('.', appendLF=FALSE)

#Testing no.apply
test_no.applyBS<-lapply(test_rangeBS, no.apply)
test_no.applyBSR<-lapply(test_rangeBSR, no.apply)

#test
expect_is(test_no.applyBS, 'list') ; message('.', appendLF=FALSE)
expect_is(test_no.applyBSR, 'list') ; message('.', appendLF=FALSE)
expect_equal(length(test_no.applyBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(test_no.applyBSR), 9) ; message('.', appendLF=FALSE)
expect_equal(test_no.applyBS, test_rangeBS) ; message('.', appendLF=FALSE)
expect_equal(test_no.applyBSR, test_rangeBSR) ; message('.', appendLF=FALSE)

#Testing centroid.apply
test_centroid.applyBS<-lapply(test_rangeBS, centroid.apply)
test_centroid.applyBSR<-lapply(test_rangeBSR, centroid.apply)

#test
expect_is(test_centroid.applyBS, 'list') ; message('.', appendLF=FALSE)
expect_is(test_centroid.applyBSR, 'list') ; message('.', appendLF=FALSE)
expect_equal(length(test_centroid.applyBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(test_centroid.applyBSR), 9) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_centroid.applyBS)), 10) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_centroid.applyBSR)), 90) ; message('.', appendLF=FALSE)

#Testing centroid.calc
test_centroidBS<-lapply(testBS, centroid.calc)
test_centroidBSR<-lapply(testBSR, centroid.calc)

#test
expect_is(test_centroidBS, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test_centroidBS), 1) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_centroidBS)), 100) ; message('.', appendLF=FALSE)
expect_is(test_centroidBSR, "list") ; message('.', appendLF=FALSE)
expect_equal(length(test_centroidBSR), 9) ; message('.', appendLF=FALSE)
expect_equal(length(unlist(test_centroidBSR)), 540) ; message('.', appendLF=FALSE)

#Testing CI.converter
test_CI1<-CI.converter(95)
test_CI2<-CI.converter(c(50,75))

#test
expect_is(test_CI1, 'numeric') ; message('.', appendLF=FALSE)
expect_is(test_CI2, 'numeric') ; message('.', appendLF=FALSE)
expect_equal(length(test_CI1), 2) ; message('.', appendLF=FALSE)
expect_equal(length(test_CI2), 4) ; message('.', appendLF=FALSE)
expect_equal(test_CI1, c(0.025,0.975)) ; message('.', appendLF=FALSE)
expect_equal(test_CI2, c(0.125, 0.250, 0.750, 0.875)) ; message('.', appendLF=FALSE)

#Testing Disparity.measure.table
test_disparity.tableBS_median<-Disparity.measure.table(type_function=prod.apply, test_rangeBS, central_tendency=median, CI=95)
test_disparity.tableBS_mean<-Disparity.measure.table(type_function=prod.apply, test_rangeBS, central_tendency=mean, CI=95)

#test
expect_is(test_disparity.tableBS_median, 'data.frame') ; message('.', appendLF=FALSE)
expect_is(test_disparity.tableBS_mean, 'data.frame') ; message('.', appendLF=FALSE)
expect_equal(ncol(test_disparity.tableBS_median), 3) ; message('.', appendLF=FALSE)
expect_equal(ncol(test_disparity.tableBS_mean), 3) ; message('.', appendLF=FALSE)
expect_equal(nrow(test_disparity.tableBS_median), 1) ; message('.', appendLF=FALSE)
expect_equal(nrow(test_disparity.tableBS_mean), 1) ; message('.', appendLF=FALSE)
expect_equal(test_disparity.tableBS_mean[,2:3], test_disparity.tableBS_median[,2:3]) ; message('.', appendLF=FALSE)

#Wrapper

#Testing disparity
set.seed(1)
test_disparityBS<-disparity(data, bootstraps=10, rarefaction=FALSE)
set.seed(1)
test_disparityBSR<-disparity(data, bootstraps=10, rarefaction=TRUE)

expect_is(test_disparityBS, 'data.frame') ; message('.', appendLF=FALSE)
expect_is(test_disparityBSR, 'data.frame') ; message('.', appendLF=FALSE)
expect_equal(ncol(test_disparityBS), 26) ; message('.', appendLF=FALSE)
expect_equal(ncol(test_disparityBS), ncol(test_disparityBSR)) ; message('.', appendLF=FALSE)
expect_equal(nrow(test_disparityBS), 1) ; message('.', appendLF=FALSE)
expect_equal(nrow(test_disparityBSR), 9) ; message('.', appendLF=FALSE)