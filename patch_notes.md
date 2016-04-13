Patch notes
----
* 2016/??/?? - v0.2.0
  * *new* utility functions: `pair.plot`, `scale.dispRity` and `sort.dispRity`.
  * *new* function: `space.maker` for creating some multidimensional spaces!
  * *new* disparity metrics: `convhull.surface`, `convhull.volume` and `hyper.volume`
  * *new* disparity tests: `sequential.test` and `null.test`.
  * *new* `plot.dispRity` arguments: `density` for controlling the polygons density and `add` for adding plots.
  * **removed** `type_discrete` argument in `plot.dispRity` and `type` argument can now be:
  	* `continuous` disparity curves.
  	* `box` for real boxplots.
  	* `lines` for the distribution verticale lines.
  	* `polygon` for the distribution boxes.
  * minor functions corrections for specific optional arguments combinations.
  * many updates to the functions manual and vignettes.
  * `boot.matrix` and `dispRity` can now run in parallel.
  * disparity can now be calculated as a distribution (i.e. `dispRity`, `test.dispRity`, `plot.dispRity`, and `summary.dispRity` can now intake one or more distribution rather than just one or more single values of disparity; whether the data is bootstrapped or not).
  * `centroids` disparity metric can now intake a `centroid` argument for fixing the centroid point value.
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
