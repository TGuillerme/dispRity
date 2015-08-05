#TEST SET GROUP

#Testing set.group_clade
#example
tree<-read.tree(text="(((((A:1,B:1):1,C:1):1,D:1):1,E:1):1,F:1);")
tree$node.label<-seq(1:5)
test<-set.group_clade(tree, 10) #clade (ABC)

#test
#class
expect_is(test, 'character') ; message('.', appendLF=FALSE)
#length
expect_that(length(test), equals(5)) ; message('.', appendLF=FALSE)
#ABC45
expect_that(grep("A", test), equals(1)) ; message('.', appendLF=FALSE)
expect_that(grep("B", test), equals(2)) ; message('.', appendLF=FALSE)
expect_that(grep("C", test), equals(3)) ; message('.', appendLF=FALSE)
expect_that(grep("4", test), equals(4)) ; message('.', appendLF=FALSE)
expect_that(grep("5", test), equals(5)) ; message('.', appendLF=FALSE)


#Testing set.group_grade
#example
tree<-read.tree(text="(((((A:1,B:1):1,C:1):1,D:1):1,E:1):1,F:1);")
tree$node.label<-seq(1:5)
test<-set.group_grade(tree, c(7,10)) #-clade (ABC)

#test
#class
expect_is(test, 'character') ; message('.', appendLF=FALSE)
#length
expect_that(length(test), equals(5)) ; message('.', appendLF=FALSE)
#DEF12
expect_that(grep("D", test), equals(1)) ; message('.', appendLF=FALSE)
expect_that(grep("E", test), equals(2)) ; message('.', appendLF=FALSE)
expect_that(grep("F", test), equals(3)) ; message('.', appendLF=FALSE)
expect_that(grep("1", test), equals(4)) ; message('.', appendLF=FALSE)
expect_that(grep("2", test), equals(5)) ; message('.', appendLF=FALSE)


#Testing set.group_add.tax
tree<-read.tree(text="(((((A:1,B:1):1,C:1):1,D:1):1,E:1):1,F:1);")
tree$node.label<-seq(1:5)
table_em<-data.frame(row.names=c(tree$tip.label, tree$node.label), "score1"=rnorm((Ntip(tree)+Nnode(tree))), "score2"=rnorm((Ntip(tree)+Nnode(tree))))
table_ta<-data.frame(row.names=c(tree$tip.label, tree$node.label), "score1"=rnorm((Ntip(tree)+Nnode(tree))), "score2"=rnorm((Ntip(tree)+Nnode(tree))), "taxonomy.col"=rep(NA,(Ntip(tree)+Nnode(tree))))
taxa1<-set.group_clade(tree, 10)
taxa2<-set.group_grade(tree, c(7,10))
test1<-set.group_add.tax(table_em, tax.col="taxo", taxa=taxa1, name="taxa1")
test2<-set.group_add.tax(table_em, tax.col="taxonomy.col", taxa=taxa2, name="taxa2")

#test
#class
expect_is(test1, 'data.frame') ; message('.', appendLF=FALSE)
expect_is(test2, 'data.frame') ; message('.', appendLF=FALSE)
#length
expect_equal(ncol(test1), 3) ; message('.', appendLF=FALSE)
expect_equal(ncol(test2), 3) ; message('.', appendLF=FALSE)
#taxa placement
expect_that(which(test1$taxo == "taxa1"), equals(c(1,2,3,10,11))) ; message('.', appendLF=FALSE)
expect_that(which(test2$taxo == "taxa2"), equals(c(4,5,6,7,8))) ; message('.', appendLF=FALSE)
