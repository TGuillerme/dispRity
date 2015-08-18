#TESTING TIME.SERIES
source("../../R/sanitizing.R")
source("../../R/tree.age.R")
source("../../R/tree.age_fun.R")
library(ape)
library(paleotree)
library(testthat)
#Loading the data

#Change the loading data to a simplified version of Beck???
load("test_data.Rda")
tree<-test_data$tree_data
data<-test_data$ord_data_tips
FADLAD<-test_data$FADLAD_data

#TIME.SERIES.DISCRETE
source("../../R/time.series_discrete.R")
time=c(120, 80, 40)
model=NULL
inc.nodes=FALSE
verbose=FALSE

time_series<-time.series.discrete(data, tree, time, FADLAD, inc.nodes)

#test
#class is list
expect_is(time_series, "list") ; message('.', appendLF=FALSE)
#length list is 2
expect_equal(length(time_series), 2) ; message('.', appendLF=FALSE)
#elements per series
series_1<-c("Daulestes","Bulaklestes","Uchkudukodon","Asioryctes","unnamed_cimolestid","Kulbeckia","Zhangolestes","unnamed_zalambdalestid")
series_2<-c("Kennalestes","Asioryctes","Ukhaatherium","Cimolestes","Maelestes","Batodon","Zalambdalestes","Barunlestes","Gypsonictops","Oxyclaenus",
    "Protungulatum","Oxyprimus","Todralestes","Pezosiren","Tribosphenomys","Paramys","Rhombomylus","Gomphos","Mimotona","Purgatorius",
    "Plesiadapis","Notharctus","Protictis","Vulpavus","Miacis","Icaronycteris","Eoryctes")
expect_equal(rownames(time_series[[1]]), series_1) ; message('.', appendLF=FALSE)
expect_equal(rownames(time_series[[2]]), series_2) ; message('.', appendLF=FALSE)
expect_equal(ncol(time_series[[1]]), 48) ; message('.', appendLF=FALSE)
expect_equal(ncol(time_series[[2]]), 48) ; message('.', appendLF=FALSE)


#With nodes
inc.nodes=TRUE
data<-test_data$ord_data_tips_nodes

time_series<-time.series.discrete(data, tree, time, FADLAD, inc.nodes)


#test
#class is list
expect_is(time_series, "list") ; message('.', appendLF=FALSE)
#length list is 2
expect_equal(length(time_series), 2) ; message('.', appendLF=FALSE)
#elements per series
expect_equal(nrow(time_series[[1]]), 32) ; message('.', appendLF=FALSE)
expect_equal(nrow(time_series[[2]]), 47) ; message('.', appendLF=FALSE)
expect_equal(ncol(time_series[[1]]), 97) ; message('.', appendLF=FALSE)
expect_equal(ncol(time_series[[2]]), 97) ; message('.', appendLF=FALSE)


#TIME.SERIES.CONTINUOUS
source("../../R/time.series_continuous.R")
source("../../R/slice.tree.R")
source("../../R/slice.tree_fun.R")
data<-test_data$ord_data_tips_nodes
time=c(120, 80, 40)
verbose=FALSE

#DELTRAN
time_series<-time.series.continuous(data, tree, time, model="deltran", FADLAD, verbose)

#test
#class is list
expect_is(time_series, "list") ; message('.', appendLF=FALSE)
#length list is 2
expect_equal(length(time_series), 3) ; message('.', appendLF=FALSE)
#elements per series
series_1<-c("n1","n5","n11")
series_2<-c("n6","Asioryctes","n7","n8","n10","n15","n17","n19","n22","n25","n34","n39","n44","n47")
series_3<-c("n17","n23","n27","n28","n29","n31","n32","n39","n42","n44","n48","n49")
expect_equal(rownames(time_series[[1]]), series_1) ; message('.', appendLF=FALSE)
expect_equal(rownames(time_series[[2]]), series_2) ; message('.', appendLF=FALSE)
expect_equal(rownames(time_series[[3]]), series_3) ; message('.', appendLF=FALSE)
expect_equal(ncol(time_series[[1]]), 97) ; message('.', appendLF=FALSE)
expect_equal(ncol(time_series[[2]]), 97) ; message('.', appendLF=FALSE)
expect_equal(ncol(time_series[[3]]), 97) ; message('.', appendLF=FALSE)

#acctran
time_series<-time.series.continuous(data, tree, time, model="acctran", FADLAD, verbose)

series_1<-c("n2","n6","n8","n12","n16")
series_2<-c("Kennalestes","Asioryctes","Ukhaatherium","Cimolestes","Maelestes","Batodon","Zalambdalestes","Barunlestes","Gypsonictops","Leptictis","Oxyclaenus","n20","n23","n26","n29","n35","Cynocephalus","n40","Patriomanis","n45","Icaronycteris","n48")
series_3<-c("Leptictis","Dasypodidae","n24","Potamogalinae","Dilambdogale","Widanelfarasia","Rhynchocyon","Procavia","Moeritherium","Trichechus","Cynocephalus","Adapis","Patriomanis","Soricidae","Solenodon")
expect_equal(rownames(time_series[[1]]), series_1) ; message('.', appendLF=FALSE)
expect_equal(rownames(time_series[[2]]), series_2) ; message('.', appendLF=FALSE)
expect_equal(rownames(time_series[[3]]), series_3) ; message('.', appendLF=FALSE)


#TIME.SERIES
source("../../R/time.series.R")

data=test_data$ord_data_tips_nodes
tree=tree_data
method="continuous"
model="acctran"
inc.nodes=TRUE
FADLAD=test_data$FADLAD_data
verbose=FALSE

#Sanitizing
#Data
expect_error(time.series(data="A", tree, method, time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data=1, tree, method, time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data=matrix(NA, nrow=2, ncol=3), tree, method, time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
#tree
expect_error(time.series(data, tree="A", method, time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data, tree=1, method, time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data, tree=rtree(5), method, time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
#method
expect_error(time.series(data, tree, method=1, time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data, tree, method="a", time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data, tree, method=c("c","d"), time, model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
#time
expect_error(time.series(data, tree, method, time="time", model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data, tree, method, time=c(140,88,0), model, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
#model
expect_error(time.series(data, tree, method, time, model="ACCTRAN", inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data, tree, method, time, model=3, inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
expect_error(time.series(data, tree, method, time, model=c("acctran","deltran"), inc.nodes, FADLAD, verbose=FALSE)) ; message('.', appendLF=FALSE)
#FADlAD
expect_error(time.series(data, tree, method, time, model, inc.nodes, FADLAD=data.frame(nrow=2, ncol=3), verbose=FALSE)) ; message('.', appendLF=FALSE)









