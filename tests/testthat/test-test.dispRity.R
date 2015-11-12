#TESTING boot.matrix

context("test.dispRity")


#Testing internal fun
test_that("flip.ref.lapply internal fun", {
    expect_error(flip.ref.lapply(1, 2, t.test))
    expect_is(flip.ref.lapply(rnorm(15), rnorm(15), t.test), "htest")
    expect_is(lapply(replicate(3,rnorm(15), simplify=F), flip.ref.lapply, referential=rnorm(15), test=t.test), "list")
    expect_equal(length(lapply(replicate(3,rnorm(15), simplify=F), flip.ref.lapply, referential=rnorm(15), test=t.test)), 3)
})

test_that("test.list.lapply internal fun", {
    expect_error(test.list.lapply(1, 2, t.test))
    expect_is(test.list.lapply(c(1,2), replicate(2,rnorm(15), simplify=F), t.test), "htest")
    expect_is(lapply(list(c(1,2), c(1,3)), test.list.lapply, replicate(3,rnorm(15), simplify=F), t.test), "list")
    expect_equal(length(lapply(list(c(1,2), c(1,3)), test.list.lapply, replicate(3,rnorm(15), simplify=F), t.test)), 2)
})

test_that("set.sequence internal fun", {
    expect_error(set.sequence("a"))
    expect_is(set.sequence(4), "matrix")
    expect_equal(dim(set.sequence(4)), c(2,3))
    expect_equal(min(set.sequence(4)), 1)
    expect_equal(max(set.sequence(4)), 4)
})

test_that("convert.to.numeric internal fun", {
    expect_true(is.na(unlist(convert.to.numeric("a", "b"))))
    expect_is(convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2)), "list")
    expect_equal(length(convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2))), 2)
    expect_equal(unlist(convert.to.numeric(list(c("a", "b"), c("b", "a")), list("a"=1, "b"=2))), c(1,2,2,1))
})
