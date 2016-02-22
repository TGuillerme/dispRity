Patch notes
----
* 2016/??/?? - v0.2.0
  * parallel options now runs properly in `boot.matrix` and `dispRity`.
  * graphical updates in `plot.dispRity(..., type = "discrete")` (box sizes, lines precision and rarefaction curves)
  * many updates to the functions manual.
  * all dispRity functions can now run in parallel!
  * new function: `space.maker` to create some multidimensional spaces!
  * new disparity metrics: `convhull.surface`, `convhull.volume` and `hyper.volume`
  * new disparity tests: `sequential.test`, `null.test` (from Diaz et al 2015 Nature).
* 2015/12/01 - v0.1.2
  * new function: `get.dispRity` for subsampling dispRity objects
  * new function: `extract.dispRity` for extracting disparity results
  * new function: `test.dispRity` for applying tests to `dispRity` objects
  * new function: `make.metric` for helping creating your very own disparity metric
  * new metric: `hyper.volume` for measuring the morphospace hyper-ellipsoid volume
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
