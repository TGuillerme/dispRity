#TEST sanitizing

context("sanitizing")

#Testing class.check
#examples
class_1<-NA ; class(class_1) <- 'class_1'
class_2<-NA ; class(class_2) <- 'class_2'
class_3<-NA ; class(class_3) <- 'class_3'
class_list<-c("class_1", "class_2")

#Test
test_that("check.class works", {
    #class - single
    expect_null(check.class(class_1, "class_1", 'test'))
    expect_error(check.class(class_1, "class_1", 'test', errorif=TRUE))
    expect_null(check.class(class_2, "class_2", 'test'))
    expect_error(check.class(class_2, "class_2", 'test', errorif=TRUE))
    #class - multiple
    expect_that(check.class(class_1, class_list, 'test'), equals("class_1"))
    expect_that(check.class(class_2, class_list, 'test'), equals("class_2"))
    expect_error(check.class(class_3, class_list, 'test'))
})

#Testing check.length
#examples
length_1<-1
length_2<-c(1, 1)
length_3<-NA
length_4<-"1"

#Test
test_that("check.length works", {
    expect_null(check.length(length_1, '1', 'test'))
    expect_null(check.length(length_3, '1', 'test'))
    expect_null(check.length(length_4, '1', 'test'))
    expect_error(check.length(length_2, '1', 'test'))
    expect_error(check.length(length_1, '1', 'test', errorif=TRUE))
})