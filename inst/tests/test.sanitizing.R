#TEST SANITYZING

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


#Testing clean.tree
#examples
tree<-rtree(10, tip.label=LETTERS[1:10])
table<-data.frame(row.names=LETTERS[3:15], "val"=rnorm(13), "val2"=rnorm(13))
table2<-data.frame(row.names=LETTERS[1:13], "val"=rnorm(13), "val2"=rnorm(13))
test.tree<-clean.tree(tree, table)
test.tree2<-clean.tree(tree, table2)

#test
expect_equal(Ntip(test.tree), 8) ; message('.', appendLF=FALSE)
expect_equal(sort(test.tree$tip.label), LETTERS[3:10]) ; message('.', appendLF=FALSE)
expect_equal(Ntip(test.tree2), 10) ; message('.', appendLF=FALSE)
expect_equal(sort(test.tree2$tip.label), LETTERS[1:10]) ; message('.', appendLF=FALSE)
expect_output(bla<-clean.tree(tree, table, verbose=TRUE), "Dropped tips") ; message('.', appendLF=FALSE)


#Testing clean.table
#examples
tree<-rtree(10, tip.label=LETTERS[1:10])
table<-data.frame(row.names=LETTERS[3:15], "val"=rnorm(13), "val2"=rnorm(13))
table2<-data.frame(row.names=LETTERS[1:13], "val"=rnorm(13), "val2"=rnorm(13))
test.table<-clean.table(table, tree)
test.table2<-clean.table(table2, tree)

#test
expect_equal(nrow(test.table), 8) ; message('.', appendLF=FALSE)
expect_equal(row.names(test.table), LETTERS[3:10]) ; message('.', appendLF=FALSE)
expect_equal(nrow(test.table2), 10) ; message('.', appendLF=FALSE)
expect_equal(row.names(test.table2), LETTERS[1:10]) ; message('.', appendLF=FALSE)
expect_output(bla<-clean.table(table, tree, verbose=TRUE), "Dropped rows") ; message('.', appendLF=FALSE)


#Testing bin.tree
#examples
tree.bin<-rtree(10)
test_1<-bin.tree(tree.bin)
tree.non<-read.tree(text = "((a:1,b:1,c:1):1,d:1);")
suppressWarnings({
test_2<-bin.tree(tree.non)
})

#test
expect_equal(test_1, tree.bin) ; message('.', appendLF=FALSE)
expect_is(test_2, 'phylo') ; message('.', appendLF=FALSE)
expect_true(is.binary.tree(test_2)) ; message('.', appendLF=FALSE)
expect_warning(test_2<-bin.tree(tree.non)) ; message('.', appendLF=FALSE)