Patch notes
----
* 2017/01/25 - v0.3.1
  * **changed name**: `series` as a part of `dispRity` objects is now changed to `subsamples` throughout the whole package.
  * **changed name**: following the last change `time.series` becomes `time.subsamples` and `cust.series` becomes `custom.subsamples` (to avoid confusion with `custard.subsamples`!).
  * Minor bug correction for optional arguments passed to `plot`.
  * `variances`, `ranges` and `centroids` are now simplified for speed. The optional arguments sanitising is now passed to `make.metric`.
* 2017/01/25 - v0.3.0 *dispRity lite!*
  * Complete change of the `dispRity` object architecture (see more [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md)).
  * `dispRity` object utilities are now all grouped under the `?dispRity.utilities`	manual with appropriate S3 methods.
  * **removed** `rm.last.axis` argument in `boot.matrix`. It is now replaced by `dimensions`.
  * **changed argument** in `plot.dispRity`, `type = "lines"` is now replaced by `type = "line"`.
  * `sim.morpho` can now use `model = "mixed"` for using both `HKY-binary` and `Mk` in characters simulation.
* 2016/11/07 - v0.2.1 - *Simulating morphological matrices*
  * *new* utility function: `merge.time.series` for cleaning or merging time series
  * *new* vignette: `dispRity-simulate_data` on how to simulate morphological characters in `dispRity`
  * *new* function: `sim.morpho` generates morphological matrices 
  * *new* function: `check.morpho` for checking how "realistic" the simulate morphological matrices are
  * *new* utility functions: `get.contrast.matrix` and `apply.inapplicable` functions for morphological matrices
  * minor graphical functions updates
* 2016/06/15 - v0.2.0
  * *new* utility functions: `pair.plot`, `scale.dispRity` and `sort.dispRity`.
  * *new* function: `space.maker` for creating some multidimensional spaces!
  * *new* disparity metrics: `convhull.surface`, `convhull.volume` and `hyper.volume`
  * *new* disparity test `null.test`.
  * *new* `plot.dispRity` arguments: `density` for controlling the polygons density and `add` for adding plots.
  * **removed** `type_discrete` argument in `plot.dispRity` and `type` argument can now be:
  	* `continuous` disparity curves.
  	* `box` for real boxplots.
  	* `lines` for the distribution verticale lines.
  	* `polygon` for the distribution boxes.
  * minor functions corrections for specific optional arguments combinations.
  * many updates to the functions manual and vignettes.
  * some algorithm are now improved for speed
  * disparity can now be calculated as a distribution (i.e. `dispRity`, `test.dispRity`, `plot.dispRity`, and `summary.dispRity` can now intake one or more distribution rather than just one or more single values of disparity; whether the data is bootstrapped or not).
  * `dispRity` can now intake `dispRity` objects with level 2 disparity metrics.
  * `boot.matrix` and `dispRity` can now run in parallel.
  * `centroids` disparity metric can now intake a `centroid` argument for fixing the centroid point value.
  * `variances` and `ranges` disparity metrics can now intake a `k.root` argument for scaling the results.
* 2015/12/01 - v0.1.2
  * *new* function: `get.dispRity` for subsampling dispRity objects
  * *new* function: `extract.dispRity` for extracting disparity results
  * *new* function: `test.dispRity` for applying tests to `dispRity` objects
  * *new* function: `make.metric` for helping creating your very own disparity metric
  * *new* metric: `hyper.volume` for measuring the morphospace hyper-ellipsoid volume
  * `metric` argument from `dispRity` can now intake up two three functions (see `dispRity.metric` and `make.metric`)
  * many improved functions manuals and examples!
  * improved vignettes:
    * *dispRity palaeo demo* a quick demo aimed more for palaeobiologist
    * *dispRity ecology demo* a quick demo aimed more for ecologists
    * *dispRity manual* for people that want to know the package in details
    * *dispRity metrics* for explaining how the disparity metric implementation works
* 2015/10/08 - v0.1.1
  * Fixed many error/warning messages
  * `plot.dispRity` options improved (rarefaction + default)
  * `cust.series` can now intake multiple factors columns
  * Added example for ecological data
  * Changed `taxa` to `elements`
  * `boot.matrix`, `dispRity`, `summary` and `plot` now also include observed values
  * `plot` has now a `observed` option to plot the observed disparity
  * `plot` option `diversity` has been renamed `elements`
* 2015/10/01 - **v0.1.0**
  * first release!
