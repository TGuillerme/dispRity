# TODO list for `serial`


Add a serial mode for calculating disparity were the calculation is slower (`for` loop) but can be depending on previously calculated values (e.g. for `ancestral.dist`) or can be applied between subsets (e.g. for measuring the distance between groups).

TODO:

 - [ ] create a new `"serial"` class option for metrics (input `matrix1` and `matrix2`)
 - [x] `make.metric` detects serial metrics
 - [x] update test for `make.metric` for detecting serials
 - [ ] `dispRity` allows serial metrics
 - [ ] `dispRity` has a `serial` argument:this argument can be logical (default is `FALSE`; `TRUE` = sequential comparison for `chrono.subset` objects/pairwise comparisons for `custom.subset` objects) or a list of pairs of comparisons (like for `test.dispRity`'s `comparisons` argument)

 - [ ] implement `ancestral.dist` as a `"serial"` metric
 - [ ] implement `min.dist` as a `"serial"` metric

Things can be sped up by directly passing a list of comparisons (pairwise or sequential or user defined) that will be passed as a `lapply_loop` argument (or similar).

Update the `plot.dispRity` structure.

* [x] function for getting the input object param `get.data.params`
* [x] function for getting what to plot `get.plot.params`
* [ ] "observed"
* [ ] "rarefaction"
* [ ] "continuous"
* [ ] "polygon" || "line"
* [ ] "box"
* [ ] `randtest`
* [ ] `dtt`
* [ ] `model.test`
* [ ] `model.sim`
* [ ] `test.metric`
