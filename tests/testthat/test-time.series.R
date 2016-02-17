#TESTING time.series

context("time.series")

#Change the loading data to a simplified version of Beck???
load("test_data.Rda")
tree<-test_data$tree_data
data<-test_data$ord_data_tips
FADLAD<-test_data$FADLAD_data

#time.series.discrete
time=c(120, 80, 40)
model=NULL
inc.nodes=FALSE
verbose=FALSE

time_series<-time.series.discrete(data, tree, time, FADLAD, inc.nodes)

#Test
test_that("time.series.discrete works properly without nodes", {
    #class is list
    expect_is(
    	time_series, "list"
    	)
    #length list is 2
    expect_equal(
    	length(time_series), 2
    	)
    #elements per series
    series_1<-c("Daulestes","Bulaklestes","Uchkudukodon","Asioryctes","unnamed_cimolestid","Kulbeckia","Zhangolestes","unnamed_zalambdalestid")
    series_2<-c("Kennalestes","Asioryctes","Ukhaatherium","Cimolestes","Maelestes","Batodon","Zalambdalestes","Barunlestes","Gypsonictops","Oxyclaenus",
        "Protungulatum","Oxyprimus","Todralestes","Pezosiren","Tribosphenomys","Paramys","Rhombomylus","Gomphos","Mimotona","Purgatorius",
        "Plesiadapis","Notharctus","Protictis","Vulpavus","Miacis","Icaronycteris","Eoryctes")
    expect_equal(
    	rownames(time_series[[1]]), series_1
    	)
    expect_equal(
    	rownames(time_series[[2]]), series_2
    	)
    expect_equal(
    	ncol(time_series[[1]]), 48
    	)
    expect_equal(
    	ncol(time_series[[2]]), 48
    	)
})

#With nodes
inc.nodes=TRUE
data<-test_data$ord_data_tips_nodes

time_series<-time.series.discrete(data, tree, time, FADLAD, inc.nodes)

#Test
test_that("time.series.discrete works properly with nodes", {
    #class is list
    expect_is(
    	time_series, "list"
    	)
    #length list is 2
    expect_equal(
    	length(time_series), 2
    	)
    #elements per series
    expect_equal(
    	nrow(time_series[[1]]), 32
    	)
    expect_equal(
    	nrow(time_series[[2]]), 47
    	)
    expect_equal(
    	ncol(time_series[[1]]), 97
    	)
    expect_equal(
    	ncol(time_series[[2]]), 97
    	)
})


#time.series.continuous
data<-test_data$ord_data_tips_nodes
time=c(120, 80, 40)
verbose=FALSE

#DELTRAN
time_series<-time.series.continuous(data, tree, time, model="deltran", FADLAD, verbose)

#Test
test_that("time.series.continuous works properly with deltran model", {
    #class is list
    expect_is(
    	time_series, "list"
    	)
    #length list is 2
    expect_equal(
    	length(time_series), 3
    	)
    #elements per series
    series_1<-c("n1","n5","n11")
    series_2<-c("n6","Asioryctes","n7","n8","n10","n15","n17","n19","n22","n25","n34","n39","n44","n47")
    series_3<-c("n17","n23","n27","n28","n29","n31","n32","n39","n42","n44","n48","n49")
    expect_equal(
    	rownames(time_series[[1]]), series_1
    	)
    expect_equal(
    	rownames(time_series[[2]]), series_2
    	)
    expect_equal(
    	rownames(time_series[[3]]), series_3
    	)
    expect_equal(
    	ncol(time_series[[1]]), 97
    	)
    expect_equal(
    	ncol(time_series[[2]]), 97
    	)
    expect_equal(
    	ncol(time_series[[3]]), 97
    	)
})

#ACCTRAN
time_series<-time.series.continuous(data, tree, time, model="acctran", FADLAD, verbose)

#Test
test_that("time.series.continuous works properly with acctran model", {
    series_1<-c("n2","n6","n8","n12","n16")
    series_2<-c("Kennalestes","Asioryctes","Ukhaatherium","Cimolestes","Maelestes","Batodon","Zalambdalestes","Barunlestes","Gypsonictops","Leptictis","Oxyclaenus","n20","n23","n26","n29","n35","Cynocephalus","n40","Patriomanis","n45","Icaronycteris","n48")
    series_3<-c("Leptictis","Dasypodidae","n24","Potamogalinae","Dilambdogale","Widanelfarasia","Rhynchocyon","Procavia","Moeritherium","Trichechus","Cynocephalus","Adapis","Patriomanis","Soricidae","Solenodon")
    expect_equal(
    	rownames(time_series[[1]]), series_1
    	)
    expect_equal(
    	rownames(time_series[[2]]), series_2
    	)
    expect_equal(
    	rownames(time_series[[3]]), series_3
    	)
})

#time.series

data=test_data$ord_data_tips_nodes
tree=test_data$tree_data
method="continuous"
model="acctran"
inc.nodes=TRUE
FADLAD=test_data$FADLAD_data
verbose=FALSE

#Sanitizing
test_that("Sanitizing works for time.series (wrapper)", {
    #Data
    expect_error(
    	time.series(data="A", tree, method, time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data=1, tree, method, time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data=matrix(NA, nrow=2, ncol=3), tree, method, time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    #tree
    expect_error(
    	time.series(data, tree="A", method, time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data, tree=1, method, time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data, tree=rtree(5), method, time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    #method
    expect_error(
    	time.series(data, tree, method=1, time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data, tree, method="a", time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data, tree, method=c("c","d"), time, model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    #time
    expect_error(
    	time.series(data, tree, method, time="time", model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data, tree, method, time=c(140,88,0), model, inc.nodes, FADLAD, verbose=FALSE)
    	)
    #model
    expect_error(
    	time.series(data, tree, method, time, model="ACCTRAN", inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data, tree, method, time, model=3, inc.nodes, FADLAD, verbose=FALSE)
    	)
    expect_error(
    	time.series(data, tree, method, time, model=c("acctran","deltran"), inc.nodes, FADLAD, verbose=FALSE)
    	)
    #FADlAD
    expect_error(
    	time.series(data, tree, method, time, model, inc.nodes, FADLAD=data.frame(nrow=2, ncol=3), verbose=FALSE)
    	)
})

#Output
test_that("Output format is correct", {
    out_test<-time.series(data, tree, method, time, model, inc.nodes, FADLAD)
    #Class
    expect_is(
    	out_test, "dispRity"
    	)
    #Length
    expect_equal(
    	length(out_test), 3
    	)
    #Names
    expect_equal(
    	names(out_test), c("data", "elements", "series")
    	)
})


#Example TESTING
test_that("Example works", {
    data(BeckLee_tree) ; data(BeckLee_mat50) ; data(BeckLee_mat99) ; data(BeckLee_ages)
    ex1<-time.series(data = BeckLee_mat50, tree = BeckLee_tree, method = "discrete", time = c(120, 80, 40), inc.nodes = FALSE, FADLAD = BeckLee_ages)
    expect_equal(
    	length(ex1), 3
    	)
    expect_equal(
    	length(ex1[[1]]), 2
    	)
    expect_equal(
    	dim(ex1[[1]][[1]]), c(8,48)
    	)

    ex2<-time.series(data = BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = c(120, 80, 40), inc.nodes = TRUE, FADLAD = BeckLee_ages)
    expect_equal(
    	length(ex2), 3
    	)
    expect_equal(
    	length(ex2[[1]]), 2
    	)
    expect_equal(
    	dim(ex2[[1]][[1]]), c(32,97)
    	)

    ex3<-time.series(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = 5, FADLAD = BeckLee_ages)
    expect_equal(
    	length(ex3), 3
    	)
    expect_equal(
    	length(ex3[[1]]), 5
    	)
    expect_equal(
    	dim(ex3[[1]][[1]]), c(3,97)
    	)
})