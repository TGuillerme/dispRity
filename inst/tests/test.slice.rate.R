#TEST SLICE RATE

#Testing P.anc

#test
#class
expect_is(P.anc(1,0,8000), 'numeric') ; message('.', appendLF=FALSE)
#length
expect_equal(length(P.anc(1,0,8000)), 1) ; message('.', appendLF=FALSE)
#No rate
expect_equal(P.anc(1,0,8000), 1) ; message('.', appendLF=FALSE)
expect_equal(P.anc(1,0,8), 1) ; message('.', appendLF=FALSE)
#No probability
expect_equal(P.anc(0,1,8000), 0) ; message('.', appendLF=FALSE)
expect_equal(P.anc(0,1,8), 0) ; message('.', appendLF=FALSE)
#No branch length
expect_equal(P.anc(1,1,0), 1) ; message('.', appendLF=FALSE)
#Smaller probability of staying ancestor with longer branch length
expect_more_than(P.anc(1,1,0.1),P.anc(1,1,0.2)) ; message('.', appendLF=FALSE)
expect_more_than(P.anc(1,1,0.1),P.anc(1,1,2000)) ; message('.', appendLF=FALSE)


#Testing branch.state

#test
#class
expect_is(class(branch.state("a", "b", 1, 1, 1)), "character") ; message('.', appendLF=FALSE)
#length
expect_equal(length(branch.state("a", "b", 1, 1, 1)), 1) ; message('.', appendLF=FALSE)
#high rate (of staying ancestral)
set.seed(1)
expect_equal(branch.state("a", "b", 1,1,0.01), "a") ; message('.', appendLF=FALSE)
#low rate (of staying ancestral)
set.seed(1)
expect_equal(branch.state("a", "b", 1,1,100000), "b") ; message('.', appendLF=FALSE)
