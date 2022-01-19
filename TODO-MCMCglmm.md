# TODO list for MCMCglmm branch?

Thanks a lot to Andrew Beckerman, Natalie Cooper and Gavin Thomas for supporting the development of this version.


TODO: update the manual with all these new entries
 - [ ] covar entry
 - [ ] plot arguments options

TODO: updates on as.covar
 - [ ] test case when the metric is as.covar AND uses tree

 * `dispRity` objects can now contain covariance matrices as a `$covar` object. The `covar` part can be directly used for some specific metrics (usually `my_metric.covar`) and are handled by the `dispRity` function (and plot, summary, etc...) in a specific way.`$covar` contains a list of two elements `VCV` the variance covariance matrix and `loc` the coordinates of the centre of the `VCV` in space (can be left empty). `$covar` matrices are effectively treated as bootstraps.
 * *New function* `covar.plot` for plotting the `covar` content of `dispRity` objects (this is separated from `plot.dispRity` because of the many different options).
 * *New function*: `MCMCglmm.subsets` is a function that allows to convert a `MCMCglmm` object into a `dispRity` object.
 * *New metric*: `projections.between` a between group metric for applying the `projections` metric between the major covariance axis of two matrices.
 * New `dispRity.fast` function for the fastest disparity calculations at the expanses of pretty much everything this package does. This is a really situational function.
 * *New utility functions* for manipulating `MCMCglmm` objects: `MCMCglmm.traits` for extracting the number of traits, `MCMCglmm.levels` for extracting the level names, `MCMCglmm.sample` for sampling posterior IDs and `MCMCglmm.covars` for extracting variance-covariance matrices
 * *New utility functions* for `dispRity` objects with `covar` matrices: `get.covar` to extract the VCV matrices (or a subsample of them); `axes.covar` to extract the major axes of the VCV matrices and `as.covar` to transform `dispRity` metric function to use a covar object.
 * *New utility function* `match.tip.edge` for matching tip labels as factors/characters/numeric to tree edges.
 * Adding `dispRity.covar.projections` function (a wrapper for covariance projections analyses).
 * One new demo datasets: `charadriiformes`, a `data.frame` and a `phylo` object of 50 random _Charadriiformes_ species (gulls, plovers and sandpipers) from [Cooney et al 2017](https://www.nature.com/articles/d41586-021-02480-z) along with a `MCMCglmm` model with each clade as a random term.
 * Additional plot arguments `...` in all the `dispRity` plotting functions can now be targeted to a specific plotting element. When multiple elements are plot by default (e.g. lines, legend, points, etc...) it is now possible to pass a specific `...` argument to the specific plotted element using the syntax `<element>.<argument>` (e.g. `points.col = "blue"` will only apply the argument `col = "blue"` to the points).
 

## Minor

 * `custom.subsets` can now group elements using a `"factor"` vector.
 * Utility functions manuals are now grouped by topic (e.g. utilities related to `MCMCglmm` objects, `dispRity` objects in general, `dispRity` objects with subsets, ect...). It should now be much easier to find these sometimes overlooked functions.
 * Many updates and new entries in the `dispRity` manual, including a section on `covar` and `between.groups` specific analyses.
 * Improving speed for the `test.metric` (using the new official `dispRity.fast` function).
       - [ ] TODO: IMPLETMENT
 * Most core functions in the package now have a garbage memory cleaning component. This improves the speed and the memory footprint wen handling very large datasets.
 * Disparity results stored in `data$disparity` now don't have dimension names anymore (significantly reducing the size of `disparity` objects). However, you can always retrieve the dimensions names using `get.disparity`.
 * Updated the calculation options for `ellipse.volume`, you can now directly speci`fy one of the following methods: `"pca"` to calculate the eigen values from the ordinated matrix; `"eigen"` to directly do an eigen decomposition of the matrix (new); or `"axes"` to directly measure the axes (new); or directly provide the eigen values.


## DEPRECATED
 
 * `matrix.dispRity` and `extract.dispRity` are now deprecated. You should now use `get.matrix` and `get.disparity` respectively instead (with the same options).




