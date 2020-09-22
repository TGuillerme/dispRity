# TODO list for `between.groups`

 - [x] create a new `"between.groups"` class option for metrics (input `matrix1` and `matrix2`)
 - [x] `make.metric` detects between.groups metrics
 - [x] update test for `make.metric` for detecting between.groupss
 - [x] `dispRity` allows between.groups metrics
 - [x] `dispRity` has a `between.groups` argument:this argument can be logical (default is `FALSE`; `TRUE` = sequential comparison for `chrono.subset` objects/pairwise comparisons for `custom.subset` objects) or a list of pairs of comparisons (like for `test.dispRity`'s `comparisons` argument)
 - [x] improve speed for the `dispRity` function: the `decompose` function in `decompose.matrix` should be isolated and switch between two versions (between groups or not).
 - [x] implement `point.dist` as a `"between.groups"` metric (add to NAMESPACE, + list of metrics in dispRity.metric (dimlevel3))
 - [x] add `point.dist` manual + example + test
 - [x] implement `group.dist` as a `"between.groups"` metric
 - [x] add `group.dist` manual + example
 - [x] add the between.groups explanation to the manual
 - [ ] fix plot bug in line 82 from `test-between.groups`

Update the `plot.dispRity` structure.

* [x] function for getting the input object param `get.data.params`
* [x] function for getting what to plot `get.plot.params`
* [x] "observed"
* [x] "rarefaction"
* [x] "continuous"
* [x] "polygon" || "line"
* [x] "box"
* [x] "preview"
* [x] specific.args
* [x] `randtest`
* [x] `dtt`
* [x] `model.test`
* [x] `model.sim`
* [x] `test.metric`
* [x] retest examples
* [x] `specific.args` for legend in `plot.preview`, `plot.randtest` and `plot.test.metric`.
* [x] `specific.args` for legend in manual.

TODO others:

 * [ ] Fix multiple states estimations in `multi.ace` for polymorphic or unknown states. The likelihood for a four possible state character should be `c(1,1,1,1)` when unknown not `c(1/4, 1/4, 1/4, 1/4)`.
 * [x] Allow non-scaled `dispRity.dtt` function
 * [ ] Allow `model.sim` to include the model name when using a `model.test` object (from previous call) - handle that correctly in the plot as well.
 * [x] Fix `Claddis.ordination` tests
 * [ ] Increase test coverage
 * [ ] Fix minor bugs in `model.test.wrapper` (see 03-model-fitting in disparity_model_fit project)
 * [x] Add disclaimer in code and manual for `model.test`.