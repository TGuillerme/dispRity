#TEST SANITYZING
source("../../R/sanitizing.R")
library(testthat)
#Testing class.check
#examples
class_1<-NA ; class(class_1) <- 'class_1'
class_2<-NA ; class(class_2) <- 'class_2'
class_3<-NA ; class(class_3) <- 'class_3'
class_list<-c("class_1", "class_2")

#test
#class - single
expect_null(check.class(class_1, "class_1", 'test')) ; message('.', appendLF=FALSE)
expect_error(check.class(class_1, "class_1", 'test', errorif=TRUE)) ; message('.', appendLF=FALSE)
expect_null(check.class(class_2, "class_2", 'test')) ; message('.', appendLF=FALSE)
expect_error(check.class(class_2, "class_2", 'test', errorif=TRUE)) ; message('.', appendLF=FALSE)
#class - multiple
expect_that(check.class(class_1, class_list, 'test'), equals("class_1")) ; message('.', appendLF=FALSE)
expect_that(check.class(class_2, class_list, 'test'), equals("class_2")) ; message('.', appendLF=FALSE)
expect_error(check.class(class_3, class_list, 'test')) ; message('.', appendLF=FALSE)

#Testing check.length
#examples
length_1<-1
length_2<-c(1, 1)
length_3<-NA
length_4<-"1"

#test
expect_null(check.length(length_1, '1', 'test')) ; message('.', appendLF=FALSE)
expect_null(check.length(length_3, '1', 'test')) ; message('.', appendLF=FALSE)
expect_null(check.length(length_4, '1', 'test')) ; message('.', appendLF=FALSE)
expect_error(check.length(length_2, '1', 'test')) ; message('.', appendLF=FALSE)
expect_error(check.length(length_1, '1', 'test', errorif=TRUE)) ; message('.', appendLF=FALSE)
