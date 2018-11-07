----
<!-- * 2017/10/18 - v1.0 *got you covered*  -->

<!-- 
dispRity 0.2.0 (2016-04-01)
=========================

### NEW FEATURES

  * Blabla

### MINOR IMPROVEMENTS

  * Blabla

### BUG FIXES

  * Blabla

### DEPRECATED AND DEFUNCT

  * Blabla
 -->

[Next release] dispRity v1.2.2 (2018-11-07)
=========================

### MINOR IMPROVEMENTS
  
  * `plot.dispRity` argument `observed` can now take a list of arguments to be passed `points()`.
  * `boot.matrix` now makes a error warning message when bootstrapping distance matrices (as suggested by [Dave Bapst](https://github.com/dwbapst/)!).
  * `geomorph.ordination` can now be used to simply create coordinates matrices (no ordination!) with `ordinate = FALSE` argument.
  * better internal handling of error messages.
  
### BUG FIXES

  * `plot.dispRity` with option `"box"` now correctly display plot ranges when disparity is an observed distribution.
  * `test.dispRity` handles errors messages more efficiently when disparity is an observed distribution.
  * `summary.dispRity` handles non-bootstrapped distributions display properly.
  * `geomorph.ordination` now converts `"character"` vectors into `"factors"`.
  * `adonis.dispRity` now properly handles complex formulas (with arithmetic signs).

[GitHub release] dispRity v1.2.1 *model tests* (2018-09-19)
=========================
  
### NEW FEATURES

  * CRAN release 2 (v.1.2) with the additional `Claddis.ordination` function.

[CRAN release] dispRity v1.2 *model tests* (2018-09-19)
=========================

### NEW FEATURES

  * *New* functions: `model.test`, `model.test.sim` and `model.test.wrapper` for fitting models of disparity evolution through time (with associated manuals, vignettes and `S3` methods! Thanks to [Mark Puttick](https://github.com/PuttickMacroevolution)).
  * **New argument** in `boot.matrix`: `prob` for passing probabilities of sampling for specific elements.
  * S3 `print` method for objects of class `"dtt"` and `"dispRity"` (from `dtt.dispRity`).

### MINOR IMPROVEMENTS

  * tydiversed most of the error messages.
  * `dtt.dispRity` now allows to specify the alternative hypothesis (if `nsim > 0`).
  * `ellipse.volume` can now take an explicit eigen value vector (the eigen values are still automatically estimated correctly for PCO and MDS).
  * Improved metric checking messages from `make.metric` when dealing with optional arguments.
  * Removed cascade of warnings triggered by `plot.dispRity.dtt`.

### BUG FIXES

  * Corrected `char.diff` to properly reflect the probability of different splits between characters (thanks to [Abigail Pastore](https://github.com/aipastore)).

<!--   * `dtt.dispRity` now works with any tree (including non-ultrametric ones and fossils only ones). -->
<!--   * `dtt.dispRity` now works with time-slicing. -->

dispRity v1.1 *got CRAN* (2018-03-20)
=========================
  
### NEW FEATURES

  * CRAN release 1 with the additional `Claddis.ordination` function.

### MINOR IMPROVEMENTS

  * Added default Cailliez correction to `Claddis.ordination` function (with `add = TRUE`).
  * Improved test coverage.

[CRAN release] dispRity v1.0.3 *got CRAN* (2018-03-20)
=========================
  
### NEW FEATURES

 * First CRAN release

### MINOR IMPROVEMENTS

  * Registered `C` symbols properly.
  * Overall improvement for the `S3` methods.
  * **Changed name**: `scale.dispRity` is now `rescale.dispRity`.
  * **Changed name**: `merge.subsets` is now `combine.subsets`.
  * **Changed name**: `time.subsets` is now `chrono.subsets` - `time.subsets` can still be called as an alias for the same function.

### BUG FIXES

  * Minor bug fixes and typos in various error messages and in the manual.

### DEPRECATED AND DEFUNCT

  * Removed `Claddis.ordination` function to comply with the CRAN requirement (this function is still live in the [GitHub version](https://github.com/TGuillerme/dispRity) 1.1).

dispRity v0.5 *covered with tests* (2017-12-20)
=========================

### NEW FEATURES

  * `custom.subset` can now automatically create clade-based groups if a `phylo` object is passed to `group`.
  * *New* utility function: `extinction.subsets`, to get the list to be passed to `test.dispRity` for testing the effect of extinction.
  * *New* test function: `dtt.dispRity`, a wrapper for [`geiger::dtt`](https://github.com/mwpennell/geiger-v2). This version is slower that `geiger::dtt` but allows any univariate disparity metric!
  * *New* test function: `adonis.dispRity`, a wrapper for [`vegan::adonis`](https://github.com/vegandevs/vegan).
  * *New* utility function: `crown.stem` for separating a tree into crown and stem groups.
  * *New* disparity metric: `span.tree.length` the length of the minimum spanning tree.
  * *New* disparity metric: `pairwise.dist`: the element's pairwise distances.
  * *New* disparity metric: `radius`: the radius of each dimensions.
  * *New* disparity metric: `n.ball.volume`: the *n*-dimensional sphere or ellipsoid volume.
  * **New argument** in `time.subsets`, `model = "equal.split"` and `model = "gradual.split"` that retain the probability of being either the descendant or the ancestor. This probability is passed to `boot.matrix` .
    
### MINOR IMPROVEMENTS

  * Changed calls to `stats::dist` to `vegan::vegdist` to allow more distances to be passed through `methods` arguments.
  * `slice.tree` can now slice through a single edge.
  * Various minor speed improvements.

### BUG FIXES

  * Correct behaviour in `tree.age` to estimate ages for trees with fossils only.

### DEPRECATED AND DEFUNCT

  * **Change name** throughout the package, `subsample` is now replaced by `subset` (e.g. `time.subsamples` is now renamed `time.subsets`, `data$subsamples` is now `data$subsets`, etc...).
  * **Changed argument** in `time.subsets`, `model = "gradual"` is now replaced by `model = "proximity"` and `model = "punctuated"` is now replaced by `model = "random"`.
  
  
dispRity v0.4.1 (2017-11-13)
=========================

### NEW FEATURES

  * *New* disparity metric: `ancestral.distance` to get the distance from taxa/nodes to their ancestors.
  * *New* function: `random.circle` for generating random circle coordinates (see example in `space.maker` for creating doughnut spaces!).
  * *New* function: `get.bin.ages` for getting the geological timescale of a tree (based on `geoscale`).

### MINOR IMPROVEMENTS

  * Added a `t0` argument to `time.subsamples` allowing to set the start age of the first subsample.
  * Allowing subsamples to contain less than three elements (up to 0!).

### BUG FIXES

  * Fixed fuzzy match issues in `slice.tree`.


dispRity v0.4 *user friendly* (2017-08-21)
=========================

### NEW FEATURES

  * Entirely rewritten manual (in GitBook)!
  * *New* function: `Claddis.ordination` and `geomorph.ordination` for automatically ordinating data from `Claddis` and `geomorph` packages!
  * *New* function: `char.diff` for calculating character differences and associated plot function (`plot.char.diff`)
  * *New* utility function: `merge.subsamples` for... merging subsamples.
  * *New* utility function: `size.subsamples` for getting the size of subsamples in a disparity object.
  * *New* wrapping functions: `dispRity.through.time` and `dispRity.per.group` now runs easy default disparity analysis.

### MINOR IMPROVEMENTS

  * Input ordinated matrices do not need to be of maximum size `n*(n-1)`. Bigger matrices now only trigger a warning.
  * Added `dimensions` optional argument to `dispRity` to overwrite the number of dimensions generated by `boot.matrix`.
  * `variances`, `ranges` and `centroids` are now simplified for speed. The optional arguments for data cleaning are now passed to `make.metric`.
  * `space.maker` now allows to approximate the dimensions variance distribution with the `scree` option.

### DEPRECATED AND DEFUNCT

  * **Removed** `hyper.volume` metric for dependencies reasons,
  * **Removed** `parallel` option from `boot.matrix` (the new architecture is already super fast: <2sec for 5k taxa and 10k bootstraps!).
  * **Changed name**: `series` as a part of `dispRity` objects is now changed to `subsamples` throughout the whole package.
  * **Changed name**: `time.series` is now renamed `time.subsamples`, if dates are provided and method is `discrete`, this function doesn't need a phylogeny any more.
  * **Changed name**: `get.subsamples.dispRity` is now renamed `get.subsamples`.
  * **Modified function**: `cust.series` is now renamed `custom.subsamples` (to avoid confusion with `custard.subsamples`!). Its `factor` argument as been changed to `groups` and can now take a simple list.


dispRity v0.3 *dispRity lite* (2017-01-25)
=========================

### NEW FEATURES

  * Complete change of the `dispRity` object architecture (see more [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md)).

### MINOR IMPROVEMENTS

  * `sim.morpho` can now use `model = "mixed"` for using both `HKY-binary` and `Mk` in characters simulation.
  * `dispRity` object utilities are now all grouped under the `?dispRity.utilities` manual with appropriate S3 methods.
  
### DEPRECATED AND DEFUNCT

  * **Removed** `rm.last.axis` argument in `boot.matrix`. It is now replaced by `dimensions`.
  * **Changed argument** in `plot.dispRity`, `type = "lines"` is now replaced by `type = "line"`.

dispRity v0.2.1 *making stuff up* (2016-11-07)
=========================

### NEW FEATURES

  * *New* utility function: `merge.time.series` for cleaning or merging time series,.
  * *New* vignette: `dispRity-simulate_data` on how to simulate morphological characters in `dispRity`.
  * *New* function: `sim.morpho` generates morphological matrices.
  * *New* function: `check.morpho` for checking how "realistic" the simulate morphological matrices are.
  * *New* utility functions: `get.contrast.matrix` and `apply.inapplicable` functions for morphological matrices.

### MINOR IMPROVEMENTS

  * Minor updates for the graphical functions.

dispRity v0.2 (2016-06-15)
=========================

### NEW FEATURES

  * *New* utility functions: `pair.plot`, `scale.dispRity` and `sort.dispRity`.
  * *New* function: `space.maker` for creating some multidimensional spaces!
  * *New* disparity metrics: `convhull.surface`, `convhull.volume` and `hyper.volume`.
  * *New* disparity test `null.test`.
  * *New* `plot.dispRity` arguments: `density` for controlling the polygons density and `add` for adding plots.

### MINOR IMPROVEMENTS

  * Many updates to the functions manual and vignettes.
  * Some algorithm are now greatly improved for speed.
  * Disparity can now be calculated as a distribution (i.e. `dispRity`, `test.dispRity`, `plot.dispRity`, and `summary.dispRity` can now intake one or more distribution rather than just one or more single values of disparity; whether the data is bootstrapped or not).
  * `dispRity` can now intake `dispRity` objects with level 2 disparity metrics.
  * `boot.matrix` and `dispRity` can now run in parallel.
  * `centroids` disparity metric can now use a `centroid` argument for fixing the centroid point value.
  * `variances` and `ranges` disparity metrics can now intake a `k.root` argument for scaling the results.

### BUG FIXES

  * Minor functions corrections for specific optional arguments combinations.

### DEPRECATED AND DEFUNCT

  * **Removed** `type_discrete` argument in `plot.dispRity` and `type` argument can now be:
    * `continuous` disparity curves.
    * `box` for real boxplots.
    * `lines` for the distribution vertical lines.
    * `polygon` for the distribution boxes.
    
dispRity v0.1.2 (2015-12-01)
=========================

### NEW FEATURES

  * *New* function: `get.dispRity` for subsampling dispRity objects.
  * *New* function: `extract.dispRity` for extracting disparity results.
  * *New* function: `test.dispRity` for applying tests to `dispRity` objects.
  * *New* function: `make.metric` for helping creating your very own disparity metric.
  * *New* metric: `hyper.volume` for measuring the morphospace hyper-ellipsoid volume.

### MINOR IMPROVEMENTS

  * `metric` argument from `dispRity` can now intake up two three functions (see `dispRity.metric` and `make.metric`).
  * Many improved functions manuals and examples!
  * Improved vignettes:
    * *dispRity palaeo demo* a quick demo aimed more for palaeobiologist.
    * *dispRity ecology demo* a quick demo aimed more for ecologists.
    * *dispRity manual* for people that want to know the package in details.
    * *dispRity metrics* for explaining how the disparity metric implementation works.
    
dispRity v0.1.1 (2015-10-08)
=========================

### MINOR IMPROVEMENTS

  * `plot.dispRity` options improved (rarefaction + default).
  * `cust.series` can now intake multiple factors columns.
  * Added example for ecological data.
  * `boot.matrix`, `dispRity`, `summary` and `plot` now also include observed values.
  * `plot` now has an `observed` option to plot the observed disparity.

### BUG FIXES

  * Fixed many error/warning messages.
  
### DEPRECATED AND DEFUNCT

  * Changed `taxa` to `elements`.
  * `plot` option `diversity` has been renamed `elements`.


dispRity v0.1 (2015-10-01)
=========================
  * First release!
