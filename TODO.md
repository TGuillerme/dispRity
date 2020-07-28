# TODO list for `between.groups`


Add a between.groups mode for calculating disparity were the calculation is slower (`for` loop) but can be depending on previously calculated values (e.g. for `ancestral.dist`) or can be applied between subsets (e.g. for measuring the distance between groups).

TODO:

 - [x] create a new `"between.groups"` class option for metrics (input `matrix1` and `matrix2`)
 - [x] `make.metric` detects between.groups metrics
 - [x] update test for `make.metric` for detecting between.groupss
 - [x] `dispRity` allows between groups metrics
 - [x] `dispRity` has a `between.groups` argument: this argument can be logical (default is `FALSE`; `TRUE` = sequential comparison for `chrono.subset` objects/pairwise comparisons for `custom.subset` objects) or a list of pairs of comparisons (like for `test.dispRity`'s `comparisons` argument)
 - [ ] test `between.groups` for `custom.subsets` (run, print, summary and plot)
 - [ ] test `between.groups` for `chrono.subsets` (run, print, summary and plot)
 - [ ] test `between.groups` for complex metric levels (decompose matrix with between groups + normal metrics & decompose matrix with normal + normal metrics)

 - [ ] implement `ancestral.dist` as a `"between.groups"` metric
 - [ ] implement `group.dist` as a `"between.groups"` metric (include type option: `centroid`, `min`, `max`, `average`, `quantile`)
 - [ ] implement `group.overlap` as a `"between.groups"` metric (generalisation of `bhat.coeff`).

Things can be sped up by directly passing a list of comparisons (pairwise or sequential or user defined) that will be passed as a `lapply_loop` argument (or similar).